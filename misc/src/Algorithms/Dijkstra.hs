------------------------------------------------------------------------------------------------------------------------
-- A* and Dijkstra's Shortest Path Algorithm
------------------------------------------------------------------------------------------------------------------------
module Algorithms.Dijkstra (Node(..), Path, Rep,
                            mkNode, initNodes, addNeis,

                            getNode, updNode, withNode,
                            astar, dijkstra)
where

  import qualified Data.Map as M
  import           Data.Map (Map)
  import           Data.Functor ((<$>))
  import           Data.List    (delete, sort)
  import           Data.Maybe (fromJust)
 
  ----------------------------------------------------------------------------------------------------------------------
  -- Data Type representing a node in the graph
  ----------------------------------------------------------------------------------------------------------------------
  data Node a = N {
                  nodVal  :: a,        -- the application data type represented by one node
                  nodDst  :: Integer,  -- the distance to the target
                  nodNei  :: [a],      -- the neighbours of this node
                  nodVis  :: Bool,     -- node was visited
                  nodPath :: Maybe a}  -- the node belongs to a path and this is the next node on the way
    deriving (Show)

  instance (Eq a) => Eq (Node a) where
    (N x _ _ _ _) == (N y _ _ _ _) = x == y

  instance (Ord a) => Ord (Node a) where
    compare (N a _ _ _ _) (N b _ _ _ _) = compare a b

  ----------------------------------------------------------------------------------------------------------------------
  -- Standard constructor
  ----------------------------------------------------------------------------------------------------------------------
  mkNode :: a -> Node a
  mkNode x = N x infinity [] False Nothing 

  ----------------------------------------------------------------------------------------------------------------------
  -- Init nodes repository from list of nodes
  ----------------------------------------------------------------------------------------------------------------------
  initNodes :: (Ord a) => [Node a] -> Rep a
  initNodes ns = M.fromList $ zip (nodVal <$> ns) ns

  ----------------------------------------------------------------------------------------------------------------------
  -- Add neighbours to node in repository    
  ----------------------------------------------------------------------------------------------------------------------
  addNeis :: (Ord a) => Rep a -> Node a -> [a] -> Rep a
  addNeis r n ns = let i = nodVal n
                    in withNode r i $ \k -> updNode r i k{nodNei = ns}

  ----------------------------------------------------------------------------------------------------------------------
  -- Integer with Infinity for the weight
  ----------------------------------------------------------------------------------------------------------------------
  infinity :: Integer
  infinity = -1

  cmp :: Integer -> Integer -> Ordering
  cmp (-1) (-1) = EQ
  cmp (-1) _    = GT
  cmp _    (-1) = LT
  cmp a    b    = compare a b

  ----------------------------------------------------------------------------------------------------------------------
  -- WIdentifier data structure to represent
  -- a list of weighted Identifiers
  -- where elements are compared for ordering by weight and 
  --                             for equality by identifier
  ----------------------------------------------------------------------------------------------------------------------
  data WIdentifier a = W Integer a
    deriving (Show)

  instance (Eq a) => Eq (WIdentifier a) where
    (W _ x) == (W _ y) = x == y

  instance (Eq a) => Ord (WIdentifier a) where
    compare (W i _) (W j _) = cmp i j

  type WQueue a = [WIdentifier a]

  ----------------------------------------------------------------------------------------------------------------------
  -- Maintain list ordered
  ----------------------------------------------------------------------------------------------------------------------
  rebalance :: (Eq a) => WIdentifier a -> WQueue a -> WQueue a
  rebalance _ [] = [] -- do not add if not in!
  rebalance p (x:xs) | p > x     = x : rebalance p xs
                     | otherwise = p : delete p (x:xs)

  ----------------------------------------------------------------------------------------------------------------------
  -- Useful type synonym
  ----------------------------------------------------------------------------------------------------------------------
  type Path a     = [Node a]

  ----------------------------------------------------------------------------------------------------------------------
  -- Node repository
  ----------------------------------------------------------------------------------------------------------------------
  type Rep a   = Map a (Node a)

  getNode :: (Ord a) => Rep a -> a -> Node a
  getNode r i = fromJust $ M.lookup i r

  updNode :: (Ord a) => Rep a -> a -> Node a -> Rep a
  updNode r i n = M.update (const $ Just n) i r

  withNode :: (Ord a) => Rep a -> a -> (Node a -> r) -> r
  withNode r i f = f (getNode r i)

  -----------------------------------------------------------------------------------------------------------------------
  -- Helper for setting up the algorithm: build a queue that is kept sorted by distance.
  -----------------------------------------------------------------------------------------------------------------------
  buildWQueue :: (Eq a, Ord a) => a -> [a] -> WQueue a
  buildWQueue s ns = sort (makeWInteger <$> ns)
    where makeWInteger i | i == s    = W 0        i -- the starting node has distance zero
                         | otherwise = W infinity i -- all others have distance infinity
 
  ----------------------------------------------------------------------------------------------------------------------
  -- Run A* shortest path algorithm
  ----------------------------------------------------------------------------------------------------------------------
  astar  :: (Eq a, Ord a) => (Node a -> Bool)                -> -- terminator
                             (Node a -> Node a   -> Integer) -> -- distance
                             (Node a -> Integer)             -> -- heuristics
                             Rep a   -> a        -> Path a
  astar e d h r s = let wq = buildWQueue s (fst <$> M.toList r)
                        r' = updNode r s (getNode r s){nodDst = 0} -- update start node to have distance 0
                     in case M.lookup s r of
                          Nothing -> []
                          Just k  -> shortestPath e d h r' wq k

  ----------------------------------------------------------------------------------------------------------------------
  -- Dijkstra is a special case of A*
  ----------------------------------------------------------------------------------------------------------------------
  dijkstra :: (Eq a, Ord a) => (Node a -> Bool)                -> -- terminator
                               (Node a -> Node a   -> Integer) -> -- distance
                                Rep a  -> a        -> Path a
  dijkstra e g = astar e g (const 0)

  ----------------------------------------------------------------------------------------------------------------------
  -- A* shortest path algorithm
  ----------------------------------------------------------------------------------------------------------------------
  shortestPath :: (Eq a, Ord a) => (Node a -> Bool)                -> -- terminator
                                   (Node a -> Node a   -> Integer) -> -- distance
                                   (Node a -> Integer)             -> -- heuristics
                                   Rep a   -> WQueue a -> Node a   -> Path a
  shortestPath _   _ _ _ [] _       = []
  shortestPath e g h r (W _ i:is) s = withNode r i $ \n -> -- visit it
                                        let (is',r') = visit g h r is i n 
                                         in if e n then reverse $ makePath r' s n
                                                   else shortestPath e g h r' is' s

  ----------------------------------------------------------------------------------------------------------------------
  -- Visit Node
  ----------------------------------------------------------------------------------------------------------------------
  visit :: (Eq a, Ord a) => (Node a -> Node a -> Integer) -> 
                            (Node a -> Integer)           ->
                            Rep a   -> WQueue a -> a      -> 
                            Node a  -> (WQueue a, Rep a)
  visit g h r wq i n | nodVis n  = (wq, r)
                     | otherwise = go r wq (nodDst n) (nodNei n)
    where go r' wq' _ []     = (wq', updNode r' (nodVal n) n)
          go r' wq' w (k:ks) = withNode r' k $ \n' ->
            let w' | w == infinity = 0
                   | otherwise     = w
                d = w' + g n n' + h n'
             in if d `cmp` nodDst n' == LT -- we need to update the node and rebalance the queue
                  then let r''  = updNode r' k n'{nodDst  = d, 
                                                  nodPath = Just i}
                           wq'' = rebalance (W d k) wq'
                        in go r'' wq'' w ks
                  else     go r'  wq'  w ks

  ----------------------------------------------------------------------------------------------------------------------
  -- Collect solution
  ----------------------------------------------------------------------------------------------------------------------
  makePath :: (Eq a, Ord a) => Rep a -> Node a -> Node a -> Path a 
  makePath r s n | n == s    = [n]
                 | otherwise = n : case nodPath n of
                                     Nothing -> [] -- dead end - error?
                                     Just i  -> withNode r i (makePath r s)

