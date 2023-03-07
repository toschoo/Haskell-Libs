------------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Algorithms/Dijkstra.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
--
-- A* and Dijkstra's Shortest Path Algorithm.
--
-- The graph is represented as a `Data.Map` where user data are keys and nodes are their values. 
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
  -- | Node represents a node in the graph.
  ----------------------------------------------------------------------------------------------------------------------
  data Node a = N {
                  nodVal  :: a,        -- ^ The application data type represented by one node
                  nodDst  :: Integer,  -- ^ The distance to the target
                  nodNei  :: [a],      -- ^ The neighbours of this node
                  nodVis  :: Bool,     -- ^ The node has already been visited
                  nodPath :: Maybe a}  -- ^ The node belongs to a path and this is the next node on the way
    deriving (Show)

  -- | /a/ needs to derive /Eq/ 
  instance (Eq a) => Eq (Node a) where
    (N x _ _ _ _) == (N y _ _ _ _) = x == y

  -- | /a/ needs to derive /Ord/ 
  instance (Ord a) => Ord (Node a) where
    compare (N a _ _ _ _) (N b _ _ _ _) = compare a b

  ----------------------------------------------------------------------------------------------------------------------
  -- | Standard constructor for a Node that can be used to set up nodes.
  ----------------------------------------------------------------------------------------------------------------------
  mkNode :: a -> Node a
  mkNode x = N x infinity [] False Nothing 

  ----------------------------------------------------------------------------------------------------------------------
  -- | Init nodes repository (i.e. the graph) from a list of nodes, e.g.
  --
  --   > rep = initNodes [ mkNode "Paul Erdös"
  --   >                 , mkNode "Stanislaw Ulam"
  --   >                 , mkNode "Nadia Heninger"
  --   >                 , mkNode "Terence Tao"
  --   >                 ]
  ----------------------------------------------------------------------------------------------------------------------
  initNodes :: (Ord a) => [Node a] -> Rep a
  initNodes ns = M.fromList $ zip (nodVal <$> ns) ns

  ----------------------------------------------------------------------------------------------------------------------
  -- | Add neighbours to a node in an existing repository, e.g.:
  --
  --   > addNeis rep n ["Stanislaw Ulam", "Alfred Tarski"]
  --
  -- Care must be taken to ensure that the correponding nodes exist in the repository.
  -- This is not checked by the path finding functions!
  ----------------------------------------------------------------------------------------------------------------------
  addNeis :: (Ord a) => Rep a -> Node a -> [a] -> Rep a
  addNeis r n ns = let i = nodVal n
                    in withNode r i $ \k -> updNode r i k{nodNei = ns}

  ----------------------------------------------------------------------------------------------------------------------
  -- Integer with Infinity for the weight
  ----------------------------------------------------------------------------------------------------------------------
  infinity :: Integer
  infinity = -1

  ----------------------------------------------------------------------------------------------------------------------
  -- ... and how to compare such integers
  ----------------------------------------------------------------------------------------------------------------------
  cmp :: Integer -> Integer -> Ordering
  cmp (-1) (-1) = EQ
  cmp (-1) _    = GT
  cmp _    (-1) = LT
  cmp a    b    = compare a b

  ----------------------------------------------------------------------------------------------------------------------
  -- WIdentifier data structure to represent a list of weighted Identifiers
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
  -- Maintain the weighted list ordered
  ----------------------------------------------------------------------------------------------------------------------
  rebalance :: (Eq a) => WIdentifier a -> WQueue a -> WQueue a
  rebalance _ [] = [] -- do not add if not in!
  rebalance p (x:xs) | p > x     = x : rebalance p xs
                     | otherwise = p : delete p (x:xs)

  ----------------------------------------------------------------------------------------------------------------------
  -- | The result of the shortest path algorithm is a path.
  ----------------------------------------------------------------------------------------------------------------------
  type Path a     = [Node a]

  ----------------------------------------------------------------------------------------------------------------------
  -- | The node repository (a.k.a. graph) is a map of user data to node.
  ----------------------------------------------------------------------------------------------------------------------
  type Rep a   = Map a (Node a)

  ----------------------------------------------------------------------------------------------------------------------
  -- | Get a node from the repository. The function is partial! It fails when the node under the key does not exist.
  ----------------------------------------------------------------------------------------------------------------------
  getNode :: (Ord a) => Rep a -> a -> Node a
  getNode r i = fromJust $ M.lookup i r

  ----------------------------------------------------------------------------------------------------------------------
  -- | Update a node in the repository. The function is partial! It fails when the node under the key does not exist.
  ----------------------------------------------------------------------------------------------------------------------
  updNode :: (Ord a) => Rep a -> a -> Node a -> Rep a
  updNode r i n = M.update (const $ Just n) i r

  ----------------------------------------------------------------------------------------------------------------------
  -- | Apply a function to a node in the repository.
  --   The function is partial! It fails when the node under the key does not exist.
  ----------------------------------------------------------------------------------------------------------------------
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
  -- | Run the A* shortest path algorithm. The function expects a number of inputs:
  --
  --   * The terminator: a function that receives a node and decides if this is the target to which we search a path.
  --                     The function may compare user data to determine if this is the target. But it may also be
  --                     a complex computation (e.g.
  --                     the data contain a numerical value that surpasses a certain threshold).
  --
  --   * The distance calculator: a function that calculates the distance between two nodes (e.g.
  --     kilometres, travel time or ticket cost). The function may be arbitrarily complex and compute, for example,
  --     a ratio of all available information like kilometres, travel time and ticket cost. 
  --     To just count the steps between nodes, one can pass /(\_ _ -> 1)/.
  --     The nodes passed in to the function are the currently visited node and the neighbour visited next.
  -- 
  --   * The heuristics calculator: a function that computes a heuristic value for two nodes (e.g.
  --     one may use the cardinal direction to decide which neighbour to choose). It is this function that
  --     distinguishes Dijkstra from A*. A* with /(\_ _ -> 0)/ is equivalent to Dijkstra.
  --     The nodes passed in to the function are the currently visited node and the neighbour visited next.
  --
  --   * A previously created repository (which can be reused for other searches).
  --
  --   * The user data of the start node.
  --
  -- A reasonable function call may look like:
  --
  --   > astar (\n -> nodVal a == "Paul Erdös")
  --   >       (\_ _ -> 1)
  --   >       (\_ _ -> 0)
  --   >       rep
  --   >       "Terence Tao"
  --
  -- The result is the shortest path represented as list of nodes from the start node to the result node, e.g.
  -- /[1, 2, 3, 4]/ where 1 is the start node and 4 is the target node.
  --
  -- There is a number of special cases:
  --
  --  * If no path is found, the start node is missing from the result
  --
  --  * If the target node could not be found the result is the empty list
  --
  --  * If the start node does not exist the result is the empty list
  --
  --  * If start and target nodes are identical the result contains only the start node
  --
  --  * If a neighbour of any node in the graph does not exist (i.e. its corresponding node is not in the repository)
  --    the function throws an exception.
  ----------------------------------------------------------------------------------------------------------------------
  astar  :: (Eq a, Ord a) => (Node a -> Bool)                -> -- terminator
                             (Node a -> Node a   -> Integer) -> -- distance
                             (Node a -> Node a   -> Integer) -> -- heuristics
                             Rep a   -> a        -> Path a
  astar e d h r s = let wq = buildWQueue s (fst <$> M.toList r)
                        r' = updNode r s (getNode r s){nodDst = 0} -- update start node to have distance 0
                     in case M.lookup s r of
                          Nothing -> []
                          Just k  -> shortestPath e d h r' wq k

  ----------------------------------------------------------------------------------------------------------------------
  -- | Dijkstra is a special case of A*. It is implemented as:
  --
  --   > dijkstra e d = astar e d (\_ _ -> 0)
  --
  ----------------------------------------------------------------------------------------------------------------------
  dijkstra :: (Eq a, Ord a) => (Node a -> Bool)                -> -- terminator
                               (Node a -> Node a   -> Integer) -> -- distance
                                Rep a  -> a        -> Path a
  dijkstra e g = astar e g (\_ _ -> 0)

  ----------------------------------------------------------------------------------------------------------------------
  -- A* shortest path algorithm
  ----------------------------------------------------------------------------------------------------------------------
  shortestPath :: (Eq a, Ord a) => (Node a -> Bool)                -> -- terminator
                                   (Node a -> Node a   -> Integer) -> -- distance
                                   (Node a -> Node a   -> Integer) -> -- heuristics
                                   Rep a   -> WQueue a -> Node a   -> Path a
  shortestPath _   _ _ _ [] _       = []
  shortestPath e g h r (W _ i:is) s = withNode r i $ \n -> -- visit it
                                        let (is',r') = visit g h r is i n 
                                         in if e n then reverse $ makePath r' s n
                                                   else shortestPath e g h r' is' s

  ----------------------------------------------------------------------------------------------------------------------
  -- Visit Node
  ----------------------------------------------------------------------------------------------------------------------
  visit :: (Eq a, Ord a) => (Node a -> Node a -> Integer) ->  -- distance
                            (Node a -> Node a -> Integer) ->  -- heuristics
                            Rep a   -> WQueue a -> a      -> 
                            Node a  -> (WQueue a, Rep a)
  visit g h r wq i n | nodVis n  = (wq, r)
                     | otherwise = go r wq (nodDst n) (nodNei n)
    where go r' wq' _ []     = (wq', updNode r' (nodVal n) n)
          go r' wq' w (k:ks) = withNode r' k $ \n' ->
            let w' | w == infinity = 1
                   | otherwise     = w
                d = w' + g n n' + h n n'
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

