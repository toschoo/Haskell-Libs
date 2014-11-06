---------------------------------------------------------------------------
-- Dijkstra's Shortest Path Algorithm and
-- A*
---------------------------------------------------------------------------
module Algorithms.Dijkstra 
where

  import qualified Data.Map as M
  import           Data.Map   (Map)
  import           Data.Maybe (fromJust)
  import           Data.List  (delete)

  -------------------------------------------------------------------------
  -- Data Type, perhaps an external interface with a standard node type
  --            would be nice
  -------------------------------------------------------------------------
  data Node a = N {
                  nodVal  :: a,
                  nodDst  :: Integer,
                  nodNei  :: [Identifier],
                  nodPath :: Maybe Identifier}
    deriving (Show)

  instance (Eq a) => Eq (Node a) where
    (N x _ _ _) == (N y _ _ _) = x == y

  mkNode :: a -> Node a
  mkNode x = N x infinity [] Nothing

  initNodes :: [Node a] -> (Rep a, [Identifier])
  initNodes ns = let is = [1..length ns]
                  in (M.fromList $ zip is ns, is)

  addNeis :: Rep a -> Identifier -> [Identifier] -> Rep a
  addNeis r i is = withNode r i $ \k -> updNode r i k{nodNei = is}

  -------------------------------------------------------------------------
  -- Integer with Infinity
  -------------------------------------------------------------------------
  infinity :: Integer
  infinity = -1

  cmp :: Integer -> Integer -> Ordering
  cmp (-1) (-1) = EQ
  cmp (-1) _    = GT
  cmp _    (-1) = LT
  cmp a    b    = compare a b

  -------------------------------------------------------------------------
  -- Useful type synonyms
  -------------------------------------------------------------------------
  type Identifier = Int
  type Path a     = [Node a]

  -------------------------------------------------------------------------
  -- WIdentifier data structure to represent
  -- a list of weighted Identifiers
  -- where elements are compared for ordering by weight and 
  --                             for equality by identifier
  -------------------------------------------------------------------------
  data WIdentifier = W Integer Identifier
    deriving (Show)

  instance Eq WIdentifier  where
    (W _ x) == (W _ y) = x == y

  instance Ord WIdentifier where
    compare (W i _) (W j _) = compare i j

  type WQueue = [WIdentifier]

  -------------------------------------------------------------------------
  -- Maintain list ordered
  -------------------------------------------------------------------------
  rebalance :: WIdentifier -> WQueue -> WQueue 
  rebalance _ [] = [] -- do not add if not in!
  rebalance p (x:xs) | p > x     = x : rebalance p xs
                     | otherwise = p : delete p (x:xs)

  -------------------------------------------------------------------------
  -- Node repository
  -------------------------------------------------------------------------
  type Rep a   = Map Identifier (Node a)

  getNode :: Rep a -> Identifier -> Node a
  getNode r i = fromJust $ M.lookup i r

  updNode :: Rep a -> Identifier -> Node a -> Rep a
  updNode r i n = M.update (const $ Just n) i r

  withNode :: Rep a -> Identifier -> (Node a -> r) -> r
  withNode r i f = f (getNode r i)

  -------------------------------------------------------------------------
  -- A* shortest path algorithm
  -------------------------------------------------------------------------
  astar  :: (Eq a) => (Node a -> Bool)              -> 
                      (Node a -> Node a -> Integer) -> 
                      (Node a -> Integer)           ->
                      Rep a   -> Node a -> WQueue   -> Path a
  astar  _   _ _ _ _ []     = []
  astar  end g h r s (W _ i:is) = withNode r i $ \n -> 
     let (is',r') = upd g h r is i n 
      in if end n then reverse $ makePath r' s n
                  else astar end g h r' s is'

  -------------------------------------------------------------------------
  -- Dijkstra's is a special case of A*
  -------------------------------------------------------------------------
  dijkstra :: (Eq a) => (Node a -> Bool)  -> 
                        (Node a -> Node a -> Integer) -> 
                         Rep a  -> Node a -> WQueue   -> Path a
  dijkstra end g = astar end g (const 0)

  -------------------------------------------------------------------------
  -- Node update
  -------------------------------------------------------------------------
  upd :: (Eq a) => (Node a -> Node a -> Integer)   -> 
                   (Node a -> Integer)             ->
                   Rep a   -> WQueue -> Identifier -> 
                   Node a  -> (WQueue, Rep a)
  upd g h r wq i n = go r wq (nodDst n) (nodNei n)
    where go r' wq' _ []     = (wq',r')
          go r1 wq1 w (k:ks) = withNode r1 k $ \n2 ->
            let d = w + g n n2 + h n2
             in if d `cmp` nodDst n2 == LT 
                  then let r2  = updNode r1 k n2{nodDst  = d, 
                                                 nodPath = Just i}
                           wq2 = rebalance (W d k) wq1
                        in go r2 wq2 w ks
                  else     go r1 wq1 w ks

  -------------------------------------------------------------------------
  -- Collect solution
  -------------------------------------------------------------------------
  makePath :: (Eq a) => Rep a -> Node a -> Node a -> Path a 
  makePath r s n | n == s    = [n]
                 | otherwise = n : case nodPath n of
                                     Nothing -> [] -- dead end - error?
                                     Just i  -> withNode r i (makePath r s)

