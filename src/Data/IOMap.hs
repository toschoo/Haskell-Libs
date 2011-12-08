{-# LANGUAGE BangPatterns #-}
module Data.IOMap (
              -- * Map Type
              IOMap,
              -- * Construction
              newTree, fromList,
              -- ** Insertion
              insert, insList,
              -- ** Deletion
              delete, pop,
              -- * Map-like
              withAll, 
              -- * Lookups
              with, with_, lookup,
              -- * Folds
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
              copy,
              toKeyList, toValList, toPairList,
              -- * Debugging
              tree2Lines
             )
              
where

  ----------------------------------------------
  -- todo
  --  - map?
  --  - withAll: force - timeout instead?
  --  - IOMap a k -> IOMap k a
  ----------------------------------------------

  import Control.Applicative
  import Control.Concurrent
  import System.IO.Unsafe
  import System.Timeout

  import Prelude hiding (lookup)

  import qualified Data.List as L (insert)

  import Graph

  newtype IOMap a k = IOMap {runTree :: MVar (Tree a k)}

  ------------------------------------------------------------------------
  -- Constructors
  ------------------------------------------------------------------------
  newTree :: (Ord k, Show k) => IO (IOMap a k) 
  newTree = IOMap <$> newMVar Nil

  fromList :: (Ord k, Show k) => (a -> a -> IO a) -> [(k, a)] -> IO (IOMap a k)
  fromList upd l = newTree >>= \t -> (insList t upd $! l) >>= \_ -> return t

  ------------------------------------------------------------------------
  -- insert
  ------------------------------------------------------------------------
  insert :: (Ord k, Show k) => IOMap a k -> (a -> a -> IO a) -> k -> a -> IO ()
  insert t upd k x = modifyMVar_ (runTree t) ins 
    where ins n = inserT n upd k x >>= (\(t',_) -> return $! t')

  insList :: (Ord k, Show k) => IOMap a k -> (a -> a -> IO a) -> [(k, a)] -> IO ()
  insList t upd l = modifyMVar_ (runTree t) (\n -> ins n $! l)
    where ins n [] = return n
          ins n ((k, x):xs) = do
            n' <- inserT n upd k x >>= (\(t',_) -> return $! t') 
            ins n' xs

  ------------------------------------------------------------------------
  -- delete
  ------------------------------------------------------------------------
  delete :: (Ord k) => IOMap a k -> k -> IO ()
  delete t k = modifyMVar_ (runTree t) del
    where del n = let (t',_) = deleTe n k 
                   in return $! t'

  pop :: (Ord k) => IOMap a k -> k -> IO (Maybe a)
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
  -- folds
  ------------------------------------------------------------------------
  fold, fold' :: (b -> b -> b) -> b -> IOMap a k -> IO b
  fold f acc t = 
    withMVar (runTree t) (return . folT f acc)

  fold' f acc t = 
    withMVar (runTree t) (\n -> return $! folT' f acc n)

  fold2, fold2' :: (b -> b) -> (b -> b -> b) -> b -> IOMap a k -> IO b
  fold2 f combine acc t = 
    withMVar (runTree t) (return . folT2 f combine acc)

  fold2' f combine acc t = 
    withMVar (runTree t) (\n -> return $! folT2' f combine acc n)

  foldKey, foldKey' :: Ord k => (k -> b -> b -> b) -> b -> IOMap a k -> IO b
  foldKey f acc t = 
    withMVar (runTree t) (return . folTKey f acc)

  foldKey' f acc t = 
    withMVar (runTree t) (\n -> return $! folTKey' f acc n)

  foldKey2, foldKey2' :: Ord k => (k -> b -> b) -> (b -> b -> b) -> b -> IOMap a k -> IO b
  foldKey2 f combine acc t = 
    withMVar (runTree t) (return . folTKey2 f combine acc)

  foldKey2' f combine acc t = 
    withMVar (runTree t) (\n -> return $! folTKey2' f combine acc n)

  foldVal, foldVal' :: (a -> b -> b -> IO b) -> b -> IOMap a k -> IO b
  foldVal f acc t =
    withMVar (runTree t) (folTVal f acc)

  foldVal' f acc t =
    withMVar (runTree t) (\n -> do !n' <- folTVal' f acc n
                                   return n')

  foldVal2, foldVal2' :: (a -> b -> IO b) -> (b -> b -> IO b) -> b -> IOMap a k -> IO b
  foldVal2 f combine acc t =
    withMVar (runTree t) (folTVal2 f combine acc)

  foldVal2' f combine acc t =
    withMVar (runTree t) (\n -> do !n' <- folTVal2' f combine acc n
                                   return n')

  foldPair, foldPair' :: ((k, a) -> b -> b -> IO b) -> b -> IOMap a k -> IO b
  foldPair  f acc t = withMVar (runTree t) (folTPair f acc)
  foldPair' f acc t = withMVar (runTree t) (\n -> do !n' <- folTPair' f acc n
                                                     return n')

  foldPair2, foldPair2' :: ((k, a) -> b -> IO b) -> (b -> b -> IO b) -> b -> IOMap a k -> IO b
  foldPair2 f combine acc t =
    withMVar (runTree t) (folTPair2 f combine acc)

  foldPair2' f combine acc t =
    withMVar (runTree t) (\n -> do !n' <- folTPair2' f combine acc n
                                   return n')

  height :: IOMap a k -> IO Int
  height = fold2 (+1) max 0

  count :: IOMap a k -> IO Int
  count = fold2 (+1) (+) 0

  copy :: IOMap a k -> IO (IOMap a k)
  copy t = do
    n' <- foldPair mkT Nil t
    t' <- newMVar n'
    return $ IOMap t'
    where mkT (k, x) l r = do
            m <- newMVar x
            let lh = folT2 (+1) max 0 l
            let rh = folT2 (+1) max 0 r
            let b  | lh > rh   = -1
                   | lh < rh   =  1
                   | otherwise =  0
            return Node {
              nKey   = k,
              nVal   = m,
              nBal   = b,
              nLeft  = l,
              nRight = r}

  toKeyList :: (Ord k) => IOMap a k -> IO [k]
  toKeyList = foldKey ins []
    where ins k l r = l ++ k:r

  toValList :: IOMap a k -> IO [a]
  toValList = foldVal ins []
    where ins x l r = return (l ++ x:r)

  toPairList :: (Ord k) => IOMap a k -> IO [(k,a)]
  toPairList = foldPair ins []
    where ins (k, x) l r = return (l ++ (k, x):r)

  ------------------------------------------------------------------------
  -- map-like
  ------------------------------------------------------------------------
  withAll :: Bool -> IOMap a k -> (a -> IO a) -> IO ()
  withAll force t upd = readMVar (runTree t) >>= \n -> wiThAll force n upd

  ------------------------------------------------------------------------
  -- finds and actions on individual nodes
  ------------------------------------------------------------------------
  lookup :: (Ord k) => k -> IOMap a k -> IO (Maybe a)
  lookup k t = do
    mbX <- withMVar (runTree t) (\n -> return $ finT n k)
    case mbX of
      Nothing -> return Nothing
      Just x  -> Just <$> readMVar x

  with :: (Ord k) => IOMap a k -> k -> (a -> IO (a, b)) -> IO (Maybe b)
  with t k upd = do
    mbX <- withMVar (runTree t) (\n -> return $ finT n k)
    case mbX of
      Nothing -> return Nothing
      Just x  -> Just <$> modifyMVar x upd

  with_ :: (Ord k) => IOMap a k -> k -> (a -> IO a) -> IO ()
  with_ t k upd = do
    mbX <- withMVar (runTree t) (\n -> return $ finT n k)
    case mbX of
      Nothing -> return ()
      Just x  -> modifyMVar_ x upd

  ------------------------------------------------------------------------
  -- Debug
  ------------------------------------------------------------------------
  tree2Lines :: Show k => IOMap a k -> GPoint -> Int -> IO [GLine]
  tree2Lines t sp len = withMVar (runTree t) (treE2lines sp len) 

  ------------------------------------------------------------------------
  -- Tree
  ------------------------------------------------------------------------
  data Tree a k = Nil
                | Node {
                    nKey    :: k,
                    nBal    :: Int,
                    nVal    :: MVar a,
                    nLeft   :: Tree a k,
                    nRight  :: Tree a k
                  }
    deriving (Eq)

  newNode :: a -> k -> IO (Tree a k)
  newNode x k = do
    v <- newMVar x
    return Node {
             nKey    = k,
             nBal    = 0,
             nVal    = v,
             nLeft   = Nil,
             nRight  = Nil}

  swapMVar_ :: MVar a -> a -> IO ()
  swapMVar_ m x = swapMVar m x >>= (\_ -> return ())

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
     in f (nKey t) fl fr

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

  wiThAll :: Bool -> Tree a k -> (a -> IO a) -> IO ()
  wiThAll _ Nil _ = return ()
  wiThAll force n upd = do
    if force then modifyMVar_ (nVal n) upd
             else timeout 100 (modifyMVar_ (nVal n) upd) >>= \_ -> return ()
    wiThAll force (nLeft  n) upd
    wiThAll force (nRight n) upd

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

  inserT :: (Ord k, Show k) => Tree a k -> (a -> a -> IO a) -> k -> a -> IO (Tree a k, Bool)
  inserT Nil _ k x = do !n <- newNode x k
                        return (n, True)
  inserT n upd k x = 
    case compare k (nKey n) of
      EQ -> updVal upd n x >> return (n, False) 
      LT -> nInsert nLeft  nRight toLeft  (-1) 1 rRotate lRotate upd n k x 
      GT -> nInsert nRight nLeft  toRight 1 (-1) lRotate rRotate upd n k x 

  updVal :: (a -> a -> IO a) -> Tree a k -> a -> IO ()
  updVal upd n x = modifyMVar_ (nVal n) (`upd` x)

  nInsert :: (Ord k, Show k) => 
             -- the kid where I am inserting
             (Tree a k -> Tree a k)             -> 
             -- the other kid 
             (Tree a k -> Tree a k)             -> 
             -- add kid
             (Tree a k -> Tree a k -> Tree a k) ->
             -- the value representing 
             -- out of balance 
             -- into my direction
             Int                                -> 
             -- the value representing 
             -- out of balance into 
             -- the other direction
             Int                                ->
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
           | nBal n == oBal = (n' {nBal =    0}, False)
           | nBal n == 0    = (n' {nBal = tBal}, h)
           | otherwise      =
               if nBal t == tBal 
                 then let !n'' = tRotate $! n' {nBal = 0}
                       in (n'' {nBal = 0}, False)
                 else let !ot  = oKid t
                          !t'  = oRotate $! t {nBal = 
                                     if nBal ot == oBal then tBal else 0}
                          !n'' = tRotate $! (add n' t') {nBal = 
                                     if nBal ot == tBal then oBal else 0}
                       in (n'' {nBal = 0}, False)
    return r

  deleTe :: (Ord k) => Tree a k -> k -> (Tree a k, Bool)
  deleTe Nil _ = (Nil, False)
  deleTe n k   =
    case compare k (nKey n) of
      EQ -> delThis n 
      LT -> let !(!l, !h) = deleTe (nLeft n) k
                !n'       = toLeft n l
             in balance nLeft nRight toRight (-1) 1 lRotate rRotate n' h
      GT -> let !(!r, !h) = deleTe (nRight n) k
                !n'       = toRight n r
             in balance nRight nLeft toLeft 1 (-1) rRotate lRotate n' h

  delThis :: Ord k => Tree a k -> (Tree a k, Bool)
  delThis n = 
    case nRight n of
      Nil -> (nLeft n, True)
      r   -> case nLeft n of
               Nil -> (r, True)
               l   -> let !(!n', !h) = rightMost l r
                       in balance nLeft nRight toRight (-1) 1 
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
                 (!n1', !h')  = balance nRight nLeft toLeft 1 (-1) 
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
             Int                                -> 
             -- the value representing 
             -- out of balance into 
             -- the other direction
             Int                                ->
             -- the single rotation, 
             -- I usually do
             (Tree a k -> Tree a k)             ->
             -- the single rotation 
             -- that starts a double rotation
             (Tree a k -> Tree a k)             ->
             Tree a k -> Bool              -> (Tree a k, Bool)
  balance tKid oKid add tBal oBal tRotate oRotate n h
    | not  h         = (n,               False)
    | nBal n == tBal = (n {nBal = 0},    h)
    | nBal n == 0    = (n {nBal = oBal}, False)
    | otherwise      =
        let o = oKid n
         in if nBal o == oBal || nBal o == 0
              then let (b1, b2, h') = if nBal o == 0 then (oBal, tBal, False)
                                                     else (   0,    0, h)
                       !n'  = tRotate $! n  {nBal = b1}
                    in (n'  {nBal = b2}, h')
              else let !to  = tKid o
                       !o'  = oRotate (o {nBal = 
                                  if nBal to == tBal then oBal else 0})
                       !n'  = tRotate $! (add n o') {nBal = 
                                  if nBal to == oBal then tBal else 0}
                    in (n'{nBal = 0}, False)
   
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

  showTree :: Show k => IOMap a k -> IO ()
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
 
