{-# LANGUAGE BangPatterns #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Data/IOMap.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: not portable (bang patterns)
--
-- Maps from key to IO actions, similar to 'Data.Map'.
-- Since many function names (but not the type name) 
-- clash with Prelude names, this module is usually imported qualified, /e.g./
--
-- > import Data.IOMap (IOMap)
-- > import qualified Data.IOMap as IOM
-- 
-- The implementation of 'IOMap' is based on /AVL/ trees.
-- 
-- Like 'Data.Map', 'IOMap' maintains key-value-pairs.
-- Values, in contrast to 'Data.Map', 
-- are protected by an 'MVar';
-- All operations on values, therefore, are IO actions.
-- Most operations on the structure of the 'IOMap',
-- such as 'delete', 'fold', 'count', /etc./,
-- are purely functional;
-- some, however, such as 'insert', but also folds
-- implying operation on the value, are IO actions.
-- 
-- The main motivation for 'IOMap'
-- is programs working with huge sets of 
-- values on which IO actions are operating concurrently, /e.g./
-- servers managing connections in an 'IOMap'.
--  
-- The library provides some traditional interfaces
-- to build and query maps ('insert', 'lookup', /etc./),
-- but due to its purpose much more important are
-- interfaces that allow for concurrent execution of IO actions
-- on values in the 'IOMap' ('with' and 'withAll').
-------------------------------------------------------------------------------
module Data.IOMap (
              -- * IOMap Type
              IOMap,
              -- * Construction
              empty, singleton, fromList,
              -- ** Insertion
              insert, insList,
              -- ** Deletion
              delete, pop,
              -- * Map-like
              withAll, 
              -- * Lookups
              with, with_, lookup,
              -- * Folds
              -- $map_folds
              fold, fold2,
              fold', fold2',
              foldKey, foldKey2,
              foldKey', foldKey2',
              foldVal, foldVal2,
              foldVal', foldVal2',
              foldPair, foldPair2,
              foldPair', foldPair2',
              -- * Inspection
              count, height, 
              -- * Copies and Conversions 
              copy, copy',
              toKeyList,  toValList,  toPairList,
              toKeyList', toValList', toPairList',
              -- * Debugging
              tree2Lines
             )
where

  ----------------------------------------------
  -- todo
  --  - count -> IOMap
  --  - release
  ----------------------------------------------

  import Control.Applicative ((<$>))
  import Control.Concurrent
  import Control.Exception (bracket, onException)

  import System.IO.Unsafe
  import System.Timeout

  import Prelude hiding (lookup)

  import qualified Data.List as L (insert)

  import Graph

  {- $map_folds

     There is an (experimental) set of /folds/.
     Some of them - and this is why they are said to be /experimental/ -
     could be replaced by 'IOMap' being 'Foldable'.
     There are basically four kinds of folds:

      * /folds/ on the structure, without using key or value;

      * /folds/ on key without using value;

      * /folds/ on the value wihtout using the key;

      * /folds/ on key-value-pairs.

      For each of these sets of folds,
      there is the basic fold:

      > fold :: (b -> b -> b) -> b -> IOMap k a -> IO b

      This /fold/ takes a function that transforms two /b/-type arguments
      into one /b/-type result.
      The two /b/-inputs corresponds to the two successors of each node.
      The /b/-argument of fold is just the regular accumulator.

      'count' could be implemented using fold as:

      > count :: IOMap k a -> IO Int
      > count = fold c 0
      >  where c l r = 1 + (l + r)

      'height' would be:

      > height :: IOMap k a -> IO Int
      > height = fold h 0
      >  where h l r = 1 + max l r
      
      In many cases, it may be more convenient to separate
      the combination of the successors results from the application 
      of this result to the /main/ fold function.
      For this reason, a second type of fold is provided:

      > fold2 :: (b -> b) -> (b -> b -> b) -> b -> IOMap k a -> IO b

      This /fold/ is very similar to the /fold/ from 'Foldable'
      for /monoids/. Note that the combinator (/b -> b -> b/)
      could be replaced by /mappend/, the accumulator by /mempty/.

      Using 'fold2', 'count' can be implemented, much nicer, as:

      > count :: IOMap k a -> IO Int
      > count = fold2 (+1) (+) 0

      'height', now, is:

      > height :: IOMap k a -> IO Int
      > height = fold2 (+1) max 0

      Finally, for each of these variants, 
      there is a lazy and a strict version, distinguished by /'/.

      All /folds/ perform with /O(n)/.
      
  -}

  ------------------------------------------------------------------------
  -- | An IOMap from keys /k/ to values /a/
  ------------------------------------------------------------------------
  newtype IOMap k a = IOMap {runTree :: MVar (Tree a k)}

  data THead k a = THead {
                     thTree  :: Tree a k,
                     thCount :: Int} -- O(1) for count!

  ------------------------------------------------------------------------
  -- | Creates an empty IOMap; /O(1)/.
  ------------------------------------------------------------------------
  empty :: IO (IOMap k a) 
  empty = IOMap <$> newMVar Nil

  ------------------------------------------------------------------------
  -- | Creates an IOMap with a single node /(k, a)/; /O(1)/.
  ------------------------------------------------------------------------
  singleton :: k -> a -> IO (IOMap k a)
  singleton k x = newNode x k >>= \n -> IOMap <$> newMVar n

  ------------------------------------------------------------------------
  -- | Creates a new IOMap from a list of key-value pairs; 
  -- /O(n*log n-i), for i = 1 .. n - 1/.
  ------------------------------------------------------------------------
  fromList :: (Ord k) => (a -> a -> IO a) -> [(k, a)] -> IO (IOMap k a)
  fromList upd l = empty >>= \t -> (insList t upd $! l) >>= \_ -> return t

  ------------------------------------------------------------------------
  -- | Inserts a new key-value pair into an existing 'IOMap'; /O(log n)/.
  --
  --   If the key /k/ is already in the map,
  --   the function /upd/ of type
  --
  --   > a -> a -> IO a
  --
  --   is used to update the correspondingn value.
  --   The first argument of the function is the old value,
  --   the second one is the new value.
  --
  --   'insert' may block waiting on other operations to finish.
  --
  --   'insert' is always strict.
  ------------------------------------------------------------------------
  insert :: (Ord k) => IOMap k a -> (a -> a -> IO a) -> k -> a -> IO ()
  insert t upd k x = modifyMVar_ (runTree t) ins 
    where ins n = inserT n upd k x >>= (\(t',_) -> return $! t')

  ------------------------------------------------------------------------
  -- | Maps 'insert' on a list of key-value pairs;
  --   /O(n * log n-i), for i = 1 .. n - 1/.
  --
  --   The /upd/ function is used like in 'insert'.
  --
  --   'insList' may block waiting on other operations to finish.
  --
  --   'insList' is always strict.
  ------------------------------------------------------------------------
  insList :: (Ord k) => IOMap k a -> (a -> a -> IO a) -> [(k, a)] -> IO ()
  insList t upd l = modifyMVar_ (runTree t) (\n -> ins n $! l)
    where ins n [] = return n
          ins n ((k, x):xs) = do
            n' <- inserT n upd k x >>= (\(t',_) -> return $! t') 
            ins n' xs

  ------------------------------------------------------------------------
  -- | Deletes the node identified by 'k' from the 'IOMap'; /O(log n)/.
  --
  --   'delete' may block waiting on other operations to finish.
  --
  --   'delete' is always strict.
  ------------------------------------------------------------------------
  delete :: (Ord k) => IOMap k a -> k -> IO ()
  delete t k = modifyMVar_ (runTree t) del
    where del n = let (t',_) = deleTe n k 
                   in return $! t'

  ------------------------------------------------------------------------
  -- | Deletes the node identified by 'k' from the 'IOMap'
  --   and returns 'Just' its value, if the 'k' was in the map,
  --   'Nothing' otherwise.
  --   /O(2 * log n)/.
  --
  --   'pop' may block waiting on other operations to finish.
  --
  --   'pop' is always strict.
  ------------------------------------------------------------------------
  pop :: (Ord k) => IOMap k a -> k -> IO (Maybe a)
  pop t k = lookup k t >>= \mbV ->
    case mbV of
      Nothing -> return Nothing
      Just !x -> delete t k >> return (Just x)

  -- with returning a result
  -- withAll returning a result
  -- withFilter
  -- release
  -- strict folds, with, withAll

  ------------------------------------------------------------------------
  -- | Lazy /fold/ ignoring keys and values
  ------------------------------------------------------------------------
  fold :: (b -> b -> b) -> b -> IOMap k a -> IO b
  fold f acc t = 
    withMVar (runTree t) (return . folT f acc)

  ------------------------------------------------------------------------
  -- | Strict /fold/ ignoring keys and values
  ------------------------------------------------------------------------
  fold' :: (b -> b -> b) -> b -> IOMap k a -> IO b
  fold' f acc t = 
    withMVar (runTree t) (\n -> return $! folT' f acc n)

  ------------------------------------------------------------------------
  -- | Lazy, monoid-style /fold/ ignoring keys and values
  ------------------------------------------------------------------------
  fold2 :: (b -> b) -> (b -> b -> b) -> b -> IOMap k a -> IO b
  fold2 f combine acc t = 
    withMVar (runTree t) (return . folT2 f combine acc)

  ------------------------------------------------------------------------
  -- | Strict, monoid-style /fold/ ignoring keys and values
  ------------------------------------------------------------------------
  fold2' :: (b -> b) -> (b -> b -> b) -> b -> IOMap k a -> IO b
  fold2' f combine acc t = 
    withMVar (runTree t) (\n -> return $! folT2' f combine acc n)

  ------------------------------------------------------------------------
  -- | Lazy , /fold/ on keys 
  ------------------------------------------------------------------------
  foldKey :: Ord k => (k -> b -> b -> b) -> b -> IOMap k a -> IO b
  foldKey f acc t = 
    withMVar (runTree t) (return . folTKey f acc)

  ------------------------------------------------------------------------
  -- | Strict, /fold/ on keys 
  ------------------------------------------------------------------------
  foldKey' :: Ord k => (k -> b -> b -> b) -> b -> IOMap k a -> IO b
  foldKey' f acc t = 
    withMVar (runTree t) (\n -> return $! folTKey' f acc n)

  ------------------------------------------------------------------------
  -- | Lazy, monoid-style /fold/ on keys 
  ------------------------------------------------------------------------
  foldKey2 :: Ord k => (k -> b -> b) -> (b -> b -> b) -> b -> IOMap k a -> IO b
  foldKey2 f combine acc t = 
    withMVar (runTree t) (return . folTKey2 f combine acc)

  ------------------------------------------------------------------------
  -- | Strict, monoid-style /fold/ on keys 
  ------------------------------------------------------------------------
  foldKey2' :: Ord k => (k -> b -> b) -> (b -> b -> b) -> b -> IOMap k a -> IO b
  foldKey2' f combine acc t = 
    withMVar (runTree t) (\n -> return $! folTKey2' f combine acc n)

  ------------------------------------------------------------------------
  -- | Lazy /fold/ on values
  ------------------------------------------------------------------------
  foldVal :: (a -> b -> b -> IO b) -> b -> IOMap k a -> IO b
  foldVal f acc t =
    withMVar (runTree t) (folTVal f acc)

  ------------------------------------------------------------------------
  -- | Strict /fold/ on values
  ------------------------------------------------------------------------
  foldVal' :: (a -> b -> b -> IO b) -> b -> IOMap k a -> IO b
  foldVal' f acc t =
    withMVar (runTree t) (\n -> do !n' <- folTVal' f acc n
                                   return n')

  ------------------------------------------------------------------------
  -- | Lazy, monoid-style /fold/ on values
  ------------------------------------------------------------------------
  foldVal2 :: (a -> b -> IO b) -> (b -> b -> IO b) -> b -> IOMap k a -> IO b
  foldVal2 f combine acc t =
    withMVar (runTree t) (folTVal2 f combine acc)

  ------------------------------------------------------------------------
  -- | Strict, monoid-style /fold/ on values
  ------------------------------------------------------------------------
  foldVal2' :: (a -> b -> IO b) -> (b -> b -> IO b) -> b -> IOMap k a -> IO b
  foldVal2' f combine acc t =
    withMVar (runTree t) (\n -> do !n' <- folTVal2' f combine acc n
                                   return n')

  ------------------------------------------------------------------------
  -- | Lazy /fold/ on pairs
  ------------------------------------------------------------------------
  foldPair :: ((k, a) -> b -> b -> IO b) -> b -> IOMap k a -> IO b
  foldPair  f acc t = withMVar (runTree t) (folTPair f acc)

  ------------------------------------------------------------------------
  -- | Strict /fold/ on pairs
  ------------------------------------------------------------------------
  foldPair' :: ((k, a) -> b -> b -> IO b) -> b -> IOMap k a -> IO b
  foldPair' f acc t = withMVar (runTree t) (\n -> do !n' <- folTPair' f acc n
                                                     return n')

  ------------------------------------------------------------------------
  -- | Lazy, monoid-style /fold/ on pairs
  ------------------------------------------------------------------------
  foldPair2 :: ((k, a) -> b -> IO b) -> (b -> b -> IO b) -> b -> 
               IOMap k a -> IO b
  foldPair2 f combine acc t =
    withMVar (runTree t) (folTPair2 f combine acc)

  ------------------------------------------------------------------------
  -- | Strict, monoid-style /fold/ on pairs
  ------------------------------------------------------------------------
  foldPair2' :: ((k, a) -> b -> IO b) -> (b -> b -> IO b) -> b -> 
                IOMap k a -> IO b
  foldPair2' f combine acc t =
    withMVar (runTree t) (\n -> do !n' <- folTPair2' f combine acc n
                                   return n')

  ------------------------------------------------------------------------
  -- | Height of the underlying /AVL/ tree; /O(n)/.
  ------------------------------------------------------------------------
  height :: IOMap k a -> IO Int
  height = fold2 (+1) max 0

  ------------------------------------------------------------------------
  -- | Number of nodes in the map; /O(n)/.
  ------------------------------------------------------------------------
  count :: IOMap k a -> IO Int
  count = fold2 (+1) (+) 0

  ------------------------------------------------------------------------
  -- | Lazy copy; /O(n)/.
  ------------------------------------------------------------------------
  copy :: IOMap k a -> IO (IOMap k a)
  copy t = do
    n' <- foldPair mkT Nil t
    t' <- newMVar n'
    return $ IOMap t'
    where mkT (k, x) l r = do
            m <- newMVar x
            let lh = folT2 (+1) max 0 l
            let rh = folT2 (+1) max 0 r
            let b  | lh > rh   = L
                   | lh < rh   = R
                   | otherwise = N
            return Node {
              nKey   = k,
              nVal   = m,
              nBal   = b,
              nLeft  = l,
              nRight = r}

  ------------------------------------------------------------------------
  -- | Strict copy; /O(n)/.
  ------------------------------------------------------------------------
  copy' :: IOMap k a -> IO (IOMap k a)
  copy' t = do
    !n' <- foldPair' mkT Nil t
    !t' <- newMVar n'
    return $! IOMap t'
    where mkT (k, x) l r = do
            !m <- newMVar x
            let !lh = folT2 (+1) max 0 l
            let !rh = folT2 (+1) max 0 r
            let !b  | lh > rh   = L
                    | lh < rh   = R
                    | otherwise = N
            return $! Node {
              nKey   = k,
              nVal   = m,
              nBal   = b,
              nLeft  = l,
              nRight = r}

  ------------------------------------------------------------------------
  -- | Lazy conversion IOMap into ordered list of key; /O(n)/.
  ------------------------------------------------------------------------
  toKeyList :: (Ord k) => IOMap k a -> IO [k]
  toKeyList = foldKey ins []
    where ins k l r = l ++ k:r

  ------------------------------------------------------------------------
  -- | Strict conversion IOMap into ordered list of key; /O(n)/.
  ------------------------------------------------------------------------
  toKeyList' :: (Ord k) => IOMap k a -> IO [k]
  toKeyList' = foldKey' ins []
    where ins k l r = l ++ k:r

  ------------------------------------------------------------------------
  -- | Lazy conversion IOMap into list of values ordered by key; /O(n)/.
  ------------------------------------------------------------------------
  toValList :: IOMap k a -> IO [a]
  toValList = foldVal ins []
    where ins x l r = return (l ++ x:r)

  ------------------------------------------------------------------------
  -- | Strict conversion IOMap into list of values ordered by key; /O(n)/.
  ------------------------------------------------------------------------
  toValList' :: IOMap k a -> IO [a]
  toValList' = foldVal' ins []
    where ins x l r = return (l ++ x:r)

  ------------------------------------------------------------------------
  -- | Lazy conversion IOMap into list of key-values pairs ordered by key; 
  --   /O(n)/.
  ------------------------------------------------------------------------
  toPairList :: (Ord k) => IOMap k a -> IO [(k,a)]
  toPairList = foldPair ins []
    where ins (k, x) l r = return (l ++ (k, x):r)

  ------------------------------------------------------------------------
  -- | Strict conversion IOMap into list of key-values pairs ordered by key; 
  --   /O(n)/.
  ------------------------------------------------------------------------
  toPairList' :: (Ord k) => IOMap k a -> IO [(k,a)]
  toPairList' = foldPair' ins []
    where ins (k, x) l r = return (l ++ (k, x):r)

  ------------------------------------------------------------------------
  -- | Applies an IO action to all values in the map;
  --   the function may block waiting on other operations to finish.
  --   But it does not prevent other operations on the map,
  --   once it has gathered the tree.
  --   In consequence, the function may continue working 
  --   on nodes that have been removed from the tree 
  --   since the function had been started;
  --   it will also miss all nodes that were added later.
  --
  --   The function may block on single values, when
  --   another operation is currently taking place.
  --   The 'Int' argument represents a timeout in microseconds
  --   that limits the duration of one IO action and, hence,
  --   the blocking time per single value. 
  --   A negative timeout means \"wait indefinitely\".
  --   (See 'System.Timeout' for details.)
  --   Note that, if you need to prevent the timeout
  --   from interrupting the IO action, 
  --   you have to take precautions 
  --   like blocking asynchronous exceptions yourself.
  --
  --   /O(n), for n = number of nodes * timeout/.
  --   Note that there is no traditional /map/,
  --   because it cannot be implemented with bounded time
  --   and is hence against the /spirit/ of this library.
  ------------------------------------------------------------------------
  withAll :: Int -> IOMap k a -> (a -> IO a) -> IO ()
  withAll tmo t upd = readMVar (runTree t) >>= \n -> wiThAll tmo n upd

  ------------------------------------------------------------------------
  -- | Returns the value of the node with the key /k/; /O(log n)/.
  --   The function may block waiting on another operation -
  --   on the map or on the value - to finish.
  --
  --   The following two sequences are semantically equal:
  --
  --   * One:
  --
  --     > lookup k m >>= \mbX -> 
  --     >   case mbX of
  --     >     Nothing -> return ()
  --     >     Just x  -> upd x
  --
  --   * Two:
  --
  --     > with_ m k upd
  ------------------------------------------------------------------------
  lookup :: (Ord k) => k -> IOMap k a -> IO (Maybe a)
  lookup k t = do
    mbX <- withMVar (runTree t) (\n -> return $ finT n k)
    case mbX of
      Nothing -> return Nothing
      Just x  -> Just <$> readMVar x

  ------------------------------------------------------------------------
  -- | Applies the IO action /a -> IO (a, b)/ to the node
  --   with key /k/ and returns 'Just' the result of the operation;
  --   if the key is in the map and 'Nothing' otherwise.
  --   The function may block waiting for another operation to finish.
  --   But it does not prevent other operations on the map,
  --   once it has gathered the tree.
  --   In consequence, the function may continue working 
  --   on a node that was removed from the tree 
  --   since the function had been started.
  --
  --   /O(log n)/.
  ------------------------------------------------------------------------
  with :: (Ord k) => IOMap k a -> k -> (a -> IO (a, b)) -> IO (Maybe b)
  with t k upd = do
    mbX <- withMVar (runTree t) (\n -> return $! finT n k)
    case mbX of
      Nothing -> return Nothing
      Just x  -> Just <$> modifyMVar x upd

  ------------------------------------------------------------------------
  -- | Variant of 'with' that does not return a value
  ------------------------------------------------------------------------
  with_ :: (Ord k) => IOMap k a -> k -> (a -> IO a) -> IO ()
  with_ t k upd = do
    mbX <- withMVar (runTree t) (\n -> return $! finT n k)
    case mbX of
      Nothing -> return ()
      Just x  -> modifyMVar_ x upd

  ------------------------------------------------------------------------
  -- | Creates /lines/ between nodes with key and balance 
  --   that can be visualised using the /op/ program,
  --   which, in its turn, uses the SOE library.
  ------------------------------------------------------------------------
  tree2Lines :: Show k => IOMap k a -> GPoint -> Int -> IO [GLine]
  tree2Lines t sp len = withMVar (runTree t) (treE2lines sp len) 

  ------------------------------------------------------------------------
  -- Tree
  ------------------------------------------------------------------------
  data Tree a k = Nil
                | Node {
                    nKey    :: k,
                    nBal    :: Balance,
                    nVal    :: MVar a,
                    nLeft   :: Tree a k,
                    nRight  :: Tree a k
                  }
    deriving (Eq)

  data Balance = L | N | R deriving (Eq, Show)

  newNode :: a -> k -> IO (Tree a k)
  newNode x k = do
    v <- newMVar x
    return Node {
             nKey    = k,
             nBal    = N,
             nVal    = v,
             nLeft   = Nil,
             nRight  = Nil}

  folT, folT' :: (b -> b -> b) -> b -> Tree a k -> b
  folT _ acc Nil = acc
  folT f acc t   = 
    let fl = folT f acc (nLeft  t)
        fr = folT f acc (nRight t)
     in f fl fr

  folT' _ acc Nil = acc
  folT' f acc t   = 
    let !fl = folT' f acc (nLeft  t)
        !fr = folT' f acc (nRight t)
     in f fl fr

  folT2, folT2' :: (b -> b) -> (b -> b -> b) -> b -> Tree a k -> b
  folT2 _ _ acc Nil = acc
  folT2 f combine acc t   = 
    let fl = folT2 f combine acc (nLeft  t)
        fr = folT2 f combine acc (nRight t)
     in f $ combine fl fr

  folT2' _ _ acc Nil = acc
  folT2' f combine acc t   = 
    let !fl = folT2' f combine acc (nLeft  t)
        !fr = folT2' f combine acc (nRight t)
     in f $! combine fl fr

  folTKey, folTKey' :: Ord k => (k -> b -> b -> b) -> b -> Tree a k -> b
  folTKey _ acc Nil = acc
  folTKey f acc t =
    let fl = folTKey f acc (nLeft  t)
        fr = folTKey f acc (nRight t)
     in f (nKey t) fl fr

  folTKey' _ acc Nil = acc
  folTKey' f acc t =
    let !fl = folTKey' f acc (nLeft  t)
        !fr = folTKey' f acc (nRight t)
        !r  = f (nKey t) fl fr
     in r

  folTKey2, folTKey2' :: Ord k => (k -> b -> b) -> (b -> b -> b) -> b -> Tree a k -> b
  folTKey2 _ _ acc Nil = acc
  folTKey2 f combine acc t =
    let fl = folTKey2 f combine acc (nLeft  t)
        fr = folTKey2 f combine acc (nRight t)
     in f (nKey t) $ combine fl fr

  folTKey2' _ _ acc Nil = acc
  folTKey2' f combine acc t =
    let !fl = folTKey2' f combine acc (nLeft  t)
        !fr = folTKey2' f combine acc (nRight t)
     in f (nKey t) $! combine fl fr

  folTVal, folTVal' :: (a -> b -> b -> IO b) -> b -> Tree a k -> IO b
  folTVal _ acc Nil = return acc
  folTVal f acc t = do
    fl <- folTVal f acc (nLeft  t)
    fr <- folTVal f acc (nRight t)
    modifyMVar (nVal t) (\v -> do x <- f v fl fr 
                                  return (v, x))

  folTVal' _ acc Nil = return acc
  folTVal' f acc t = do
    !fl <- folTVal' f acc (nLeft  t)
    !fr <- folTVal' f acc (nRight t)
    modifyMVar (nVal t) (\v -> do !x <- f v fl fr 
                                  return (v, x))

  folTVal2, folTVal2' :: (a -> b -> IO b) -> (b -> b -> IO b) -> b -> Tree a k -> IO b
  folTVal2 _ _ acc Nil = return acc
  folTVal2 f combine acc t = do
    fl <- folTVal2 f combine acc (nLeft  t)
    fr <- folTVal2 f combine acc (nRight t)
    ff <- combine fl fr
    modifyMVar (nVal t) (\v -> do x <- f v ff 
                                  return (v, x))

  folTVal2' _ _ acc Nil = return acc
  folTVal2' f combine acc t = do
    !fl <- folTVal2' f combine acc (nLeft  t)
    !fr <- folTVal2' f combine acc (nRight t)
    !ff <- combine fl fr
    modifyMVar (nVal t) (\v -> do !x <- f v ff 
                                  return (v, x))

  folTPair :: ((k, a) -> b -> b -> IO b) -> b -> Tree a k -> IO b
  folTPair _ acc Nil = return acc
  folTPair f acc t   = do
    fl <- folTPair f acc (nLeft  t)
    fr <- folTPair f acc (nRight t)
    let k = nKey t
    modifyMVar (nVal t) (\v -> do x <- f (k, v) fl fr 
                                  return (v, x))
  folTPair' _ acc Nil = return acc
  folTPair' f acc t   = do
    !fl <- folTPair' f acc (nLeft  t)
    !fr <- folTPair' f acc (nRight t)
    let !k = nKey t
    modifyMVar (nVal t) (\v -> do !x <- f (k, v) fl fr 
                                  return (v, x))


  folTPair2, folTPair2' :: ((k, a) -> b -> IO b) -> (b -> b -> IO b) -> b -> Tree a k -> IO b
  folTPair2 _ _ acc Nil = return acc
  folTPair2 f combine acc t = do
    fl <- folTPair2 f combine acc (nLeft  t)
    fr <- folTPair2 f combine acc (nRight t)
    ff <- combine fl fr
    let k = nKey t
    modifyMVar (nVal t) (\v -> do x <- f (k, v) ff 
                                  return (v, x))

  folTPair2' _ _ acc Nil = return acc
  folTPair2' f combine acc t = do
    !fl <- folTPair2' f combine acc (nLeft  t)
    !fr <- folTPair2' f combine acc (nRight t)
    !ff <- combine fl fr
    let !k = nKey t
    modifyMVar (nVal t) (\v -> do !x <- f (k, v) ff 
                                  return (v, x))

  mapT :: (a -> IO a) -> Tree a k -> IO ()
  mapT _ Nil = return ()
  mapT f n   = do
    modifyMVar_ (nVal n) f
    mapT f (nLeft  n) 
    mapT f (nRight n) 

  wiThAll :: Int -> Tree a k -> (a -> IO a) -> IO ()
  wiThAll _ Nil _ = return ()
  wiThAll tmo n upd = do
    wiThAll tmo (nLeft  n) upd
    wiThisOne
    wiThAll tmo (nRight n) upd
    where wiThisOne = 
            bracket (timeout tmo $ takeMVar (nVal n))
                    (\mbV -> case mbV of
                               Nothing -> return ()
                               Just v  -> do
                                 v' <- onException (upd    v)
                                                   (return v) 
                                 putMVar (nVal n) v')
                    return

  finT :: (Ord k) => Tree a k -> k -> Maybe (MVar a)
  finT Nil _ = Nothing
  finT n   k = if k == nKey n then Just (nVal n)
                 else 
                   case finT (nLeft n) k of
                     Just x  -> Just x
                     Nothing -> 
                       case finT (nRight n) k of
                         Nothing -> Nothing
                         Just x  -> Just x

  inserT :: (Ord k) => Tree a k -> (a -> a -> IO a) -> k -> a -> IO (Tree a k, Bool)
  inserT Nil _ k x = do !n <- newNode x k
                        return (n, True)
  inserT n upd k x = 
    case compare k (nKey n) of
      EQ -> updVal upd n x >> return (n, False) 
      LT -> nInsert nLeft  nRight toLeft  L R rRotate lRotate upd n k x 
      GT -> nInsert nRight nLeft  toRight R L lRotate rRotate upd n k x 

  updVal :: (a -> a -> IO a) -> Tree a k -> a -> IO ()
  updVal upd n x = modifyMVar_ (nVal n) (`upd` x)

  nInsert :: (Ord k) => 
             -- the kid where I am inserting
             (Tree a k -> Tree a k)             -> 
             -- the other kid 
             (Tree a k -> Tree a k)             -> 
             -- add kid
             (Tree a k -> Tree a k -> Tree a k) ->
             -- the value representing 
             -- out of balance 
             -- into my direction
             Balance                            -> 
             -- the value representing 
             -- out of balance into 
             -- the other direction
             Balance                            -> 
             -- the single rotation, 
             -- I usually do
             (Tree a k -> Tree a k)             ->
             -- the single rotation 
             -- that starts a double rotation
             (Tree a k -> Tree a k)             ->
             (a -> a -> IO a)                   ->
             Tree a k -> k -> a                 -> IO (Tree a k, Bool)
  nInsert tKid oKid add tBal oBal tRotate oRotate upd n k x = do
    (!t, !h) <- inserT (tKid n) upd k x 
    let !n' = add n t 
    let !r | not  h         = (n',               False)
           | nBal n == oBal = (n' {nBal =    N}, False)
           | nBal n == N    = (n' {nBal = tBal}, h)
           | otherwise      =
               if nBal t == tBal 
                 then let !n'' = tRotate $! n' {nBal = N}
                       in (n'' {nBal = N}, False)
                 else let !ot  = oKid t
                          !t'  = oRotate $! t {nBal = 
                                     if nBal ot == oBal then tBal else N}
                          !n'' = tRotate $! (add n' t') {nBal = 
                                     if nBal ot == tBal then oBal else N}
                       in (n'' {nBal = N}, False)
    return r

  deleTe :: (Ord k) => Tree a k -> k -> (Tree a k, Bool)
  deleTe Nil _ = (Nil, False)
  deleTe n k   =
    case compare k (nKey n) of
      EQ -> delThis n 
      LT -> let !(!l, !h) = deleTe (nLeft n) k
                !n'       = toLeft n l
             in balance nLeft nRight toRight L R lRotate rRotate n' h
      GT -> let !(!r, !h) = deleTe (nRight n) k
                !n'       = toRight n r
             in balance nRight nLeft toLeft  R L rRotate lRotate n' h

  delThis :: Ord k => Tree a k -> (Tree a k, Bool)
  delThis n = 
    case nRight n of
      Nil -> (nLeft n, True)
      r   -> case nLeft n of
               Nil -> (r, True)
               l   -> let !(!n', !h) = rightMost l r
                       in balance nLeft nRight toRight L R
                                  lRotate rRotate n'{nBal = nBal n} h 

  rightMost :: Ord k => Tree a k -> Tree a k -> (Tree a k, Bool)
  rightMost n r =
    let (!n', !r', !h) = del n r
     in case nLeft n' of
          Nil -> (n' {nLeft = r'}, h)
          !l  -> (n' {nLeft = l {nLeft = r'}}, h)

  del :: Ord k => Tree a k -> Tree a k -> (Tree a k, Tree a k, Bool)
  del n1 n2 = 
    case nRight n1 of
      Nil -> (n1 {nRight = n2, nLeft = Nil}, nLeft n1, True)
      r   -> let (!x, !r, !h) = del (nRight n1) n2 
                 (!n1', !h')  = balance nRight nLeft toLeft R L
                                        rRotate lRotate n1{nRight = r} h
              in (x, n1',  h') -- False?

  toLeft, toRight :: Tree a k -> Tree a k -> Tree a k
  toLeft  t1 t2 = t1 {nLeft  = t2}
  toRight t1 t2 = t1 {nRight = t2} 

  balance :: (Ord k) => 
             -- the kid where I am deleting
             (Tree a k -> Tree a k)             -> 
             -- the other kid 
             (Tree a k -> Tree a k)             -> 
             (Tree a k -> Tree a k -> Tree a k) ->
             -- the value representing 
             -- out of balance 
             -- into my direction
             Balance                            -> 
             -- the value representing 
             -- out of balance into 
             -- the other direction
             Balance                            -> 
             -- the single rotation, 
             -- I usually do
             (Tree a k -> Tree a k)             ->
             -- the single rotation 
             -- that starts a double rotation
             (Tree a k -> Tree a k)             ->
             Tree a k -> Bool              -> (Tree a k, Bool)
  balance tKid oKid add tBal oBal tRotate oRotate n h
    | not  h         = (n,               False)
    | nBal n == tBal = (n {nBal = N},    h)
    | nBal n == N    = (n {nBal = oBal}, False)
    | otherwise      =
        let o = oKid n
         in if nBal o == oBal || nBal o == N
              then let (b1, b2, h') = if nBal o == N then (oBal, tBal, False)
                                                     else (   N,    N, h)
                       !n'  = tRotate $! n  {nBal = b1}
                    in (n'  {nBal = b2}, h')
              else let !to  = tKid o
                       !o'  = oRotate (o {nBal = 
                                  if nBal to == tBal then oBal else N})
                       !n'  = tRotate $! (add n o') {nBal = 
                                  if nBal to == oBal then tBal else N}
                    in (n'{nBal = N}, False)
   
  rRotate :: Tree a k -> Tree a k
  rRotate Nil = Nil
  rRotate n   = 
    let !l  = nLeft  n
        !rl = nRight l
        !n' = n {nLeft  = rl}
        !l' = l {nRight = n'} 
     in l'

  lRotate :: Tree a k -> Tree a k
  lRotate Nil = Nil
  lRotate n   = 
    let !r  = nRight n
        !lr = nLeft  r
        !n' = n {nRight = lr}
        !r' = r {nLeft  = n'} 
     in r'

  treeToLisT :: Tree a k -> IO [a]
  treeToLisT Nil = return []
  treeToLisT n   = do
    let l = nLeft n
    cns <- case l of
             Nil -> return []
             t   -> treeToLisT t
    v <- readMVar (nVal n)
    let r = nRight n
    cdr <- case r of
             Nil -> return []
             t   -> treeToLisT t
    return  (cns ++ v : cdr)

  showTree :: Show k => IOMap k a -> IO ()
  showTree t = withMVar (runTree t) (showTreE "")

  showTreE :: Show k => String -> Tree a k -> IO ()
  showTreE _ Nil = return ()
  showTreE i n   = do
    showTreE ('\t' : i) (nRight n) >> putStrLn ""
    putStr (i ++ show (nKey n)) >> putStrLn ""
    showTreE ('\t' : i) (nLeft  n) >> putStrLn ""

  ------------------------------------------------------------------------
  -- construct a graphical representation of the tree
  ------------------------------------------------------------------------
  gNode :: Show k => Tree a k -> GPoint -> GNode
  gNode Nil p = GNode {gTxt = nil, gPoint = p}
  gNode n   p = GNode {gTxt = show (nKey n) ++ " (" ++ 
                              show (nBal n) ++ ")", 
                              gPoint = p} 

  treE2lines :: Show k => GPoint -> Int -> Tree a k -> IO [GLine]
  treE2lines sp len tree = 
    toLines tree (gNode tree sp) len

  toLines :: Show k => Tree a k -> GNode -> Int -> IO [GLine]
  toLines Nil _ _      = return []
  toLines n mom len = do
    let lk = nLeft  n
    let rk = nRight n
    let (l, r) = (gNode lk
                   (getX mom - len,
                    getY mom + len),
                  gNode rk
                   (getX mom + len,
                    getY mom + len))
    lks <- toLines lk l (len `div` 2)
    rks <- toLines rk r (len `div` 2)
    return $ (mom, l) : (mom, r) : (lks ++ rks) 
 
