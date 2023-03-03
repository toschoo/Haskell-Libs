------------------------------------------------------------------------------------------------------------------------
-- KnightAndQueen provides functions that find solutions to the following problem:
-- There are only one Knight and one Queen on the board.
-- The positions of the Queen, Knight and a target square are given.
-- Move the Knight to the target square without
--  - crossing a square that is controlled by the Queen
--  - taking the Queen.
------------------------------------------------------------------------------------------------------------------------
module KnightAndQueen (ridiculousKnight, funnyKnight, knightTour)
where

  import qualified Data.Map as M
  import           Data.Map (Map)
  import           Data.Monoid ((<>))
  import           Data.Char (ord, chr)
  import           Data.Functor ((<$>))
  import           Data.List    (delete, nub)
  import           Data.Foldable (foldl')

  import           Algorithms.Dijkstra

  ----------------------------------------------------------------------------------------------------------------------
  -- ridiculousKnight finds a path between knight and target.
  -- The solution may be ridiculous, i.e. a very long path while there are shorter ones.
  -- The function is called with algebraic notation, e.g.:
  -- `ridiculousKnight "d5" "h8" "f8"` for a Queen on d5, the Knight on h8 and the target square f8.
  ----------------------------------------------------------------------------------------------------------------------
  ridiculousKnight :: String -> String -> String -> [String]
  ridiculousKnight queen knight target = label <$> reverse (
    findRoute (notAllowed $ code queen) M.empty (code knight) (code target))

  ----------------------------------------------------------------------------------------------------------------------
  -- funnyKnight finds the shortest path between knight and target according to Dijkstra's algorithm.
  -- The function is called with algebraic notation, e.g.:
  -- `funnyKnight "d5" "h8" "f8"` for a Queen on d5, the Knight on h8 and the target square f8.
  ----------------------------------------------------------------------------------------------------------------------
  funnyKnight :: String -> String -> String -> [String]
  funnyKnight queen knight target =
    let g = buildGraph $ code queen
     in (label . nodVal) <$> dijkstra end dist g (code knight)
    where end (N i _ _ _ _) = i == code target -- check if we reached the target
          dist _ _ = 1                         -- distance calculator

  ----------------------------------------------------------------------------------------------------------------------
  -- Complete Knight Tour with Queen on d5 starting on h8:
  -- h8 -> f8 -> e8 -> c8 -> b8 -> b8 -> h7, etc.
  ----------------------------------------------------------------------------------------------------------------------
  knightTour :: [[String]]
  knightTour = uncurry (funnyKnight "d5") <$> [("h8", "f8"), ("f8", "e8"), ("e8", "c8"), ("c8", "b8"),
                                               ("b8", "h7"), ("h7", "g7"), ("g7", "e7"), ("e7", "c7"), ("c7", "a7"),
                                               ("a7", "h6"), ("h6", "g6"), ("g6", "f6"), ("f6", "b6"), ("b6", "a6"),
                                               ("a6", "h4"), ("h4", "g4"), ("g4", "f4"), ("f4", "b4"), ("b4", "a4"),
                                               ("a4", "h3"), ("h3", "g3"), ("g3", "e3"), ("e3", "c3"), ("c3", "a3"),
                                               ("a3", "h2"), ("h2", "f2"), ("f2", "e2"), ("e2", "c2"), ("c2", "b2"),
                                               ("b2", "g1"), ("g1", "f1"), ("f1", "e1"), ("e1", "c1"), ("c1", "b1"),
                                               ("b1", "a1")]

  -----------------------------------------------------------------------------------------------------------------------
  -- A square is just a point in the sequence of integers 0 - 63
  -- where every 8th point is considered to be the start of a new row.
  -----------------------------------------------------------------------------------------------------------------------
  type Square = Int

  row :: Square -> Int
  row n = n `div` 8 
  
  file :: Square -> Int
  file n = n `rem` 8

  -----------------------------------------------------------------------------------------------------------------------
  -- Algebraic notation of the square (e.g. 0 -> "a1")
  -----------------------------------------------------------------------------------------------------------------------
  label :: Square -> String
  label n = 
    let x = file2alpha $ file n
        y = row2num $ row n
     in x <> show y

  -----------------------------------------------------------------------------------------------------------------------
  -- Code for a given coordinate in algebraic notation (e.g. "a1" -> 0).
  -- This function is unsafe and panics on invalid input!
  -----------------------------------------------------------------------------------------------------------------------
  code :: String -> Square
  code [x,y] = let f = ord x - 97
                   c = read [y] -- unsafe!
                   r = 8*(c-1)
                in r + f
  code _ = error "not a valid label"

  -----------------------------------------------------------------------------------------------------------------------
  -- helpers for conversion between squares and algebraic notation.
  -----------------------------------------------------------------------------------------------------------------------
  file2alpha :: Int -> String
  file2alpha n = [chr(n+97)]

  row2num :: Int -> Int
  row2num = (+1)

  -----------------------------------------------------------------------------------------------------------------------
  -- Magnus, how does the Knight move?
  -----------------------------------------------------------------------------------------------------------------------
  knightMovesFrom :: Square -> [Square]
  knightMovesFrom n =
    let (f, r) = (file n, row n)
        nne = if f < 7 && r < 6 then n + 1 + 16 else -1
        ene = if f < 6 && r < 7 then n + 2 +  8 else -1
        ese = if f < 6 && r > 0 then n + 2 -  8 else -1
        sse = if f < 7 && r > 1 then n + 1 - 16 else -1 
        ssw = if f > 0 && r > 1 then n - 1 - 16 else -1 
        wsw = if f > 1 && r > 0 then n - 2 -  8 else -1
        wnw = if f > 1 && r < 7 then n - 2 +  8 else -1
        nnw = if f > 0 && r < 6 then n - 1 + 16 else -1
     in filter (>=0) [
           nne, ene, ese, sse, ssw, wsw, wnw, nnw
        ]

  -----------------------------------------------------------------------------------------------------------------------
  -- Square controlled by the Queen.
  -----------------------------------------------------------------------------------------------------------------------
  notAllowed :: Square -> Map Square ()
  notAllowed n = let q = n : queenMovesFrom n
                     l = length q
                  in M.fromList $ zip q $ replicate l ()

  -----------------------------------------------------------------------------------------------------------------------
  -- The Queen reaches all squares that a Rook or a Bishop would reach from here.
  -----------------------------------------------------------------------------------------------------------------------
  queenMovesFrom :: Square -> [Square]
  queenMovesFrom n = nub (
                       rookMovesFrom   n <>
                       bishopMovesFrom n
                     )

  -----------------------------------------------------------------------------------------------------------------------
  -- Rook moves in its file or its row.
  -----------------------------------------------------------------------------------------------------------------------
  rookMovesFrom :: Square -> [Square]
  rookMovesFrom n =
    let (f, r) = (file n, row n)
        fs     = (+8*r) <$> [0..7]
        rs     = (+f) <$> [0,8..56]
     in delete n $ nub (fs <> rs)

  -----------------------------------------------------------------------------------------------------------------------
  -- Cardinal directions of a diagonal.
  -----------------------------------------------------------------------------------------------------------------------
  data Direction = NE | SE | SW | NW
    deriving Eq

  -----------------------------------------------------------------------------------------------------------------------
  -- Bishops move on diagonals.
  -----------------------------------------------------------------------------------------------------------------------
  bishopMovesFrom :: Square -> [Square]
  bishopMovesFrom n =  delete n $ nub (
                         diagonal NE n <>
                         diagonal SE n <>
                         diagonal SW n <>
                         diagonal NW n
                       )

  -----------------------------------------------------------------------------------------------------------------------
  -- What is a diagonal?
  -----------------------------------------------------------------------------------------------------------------------
  diagonal :: Direction -> Square -> [Square]
  diagonal d n = 
    let (f, r) = (file n, row n)
        p | d == NE && f < 7 && r < 7 = n + 1 + 8
          | d == SE && f < 7 && r > 0 = n + 1 - 8
          | d == SW && f > 0 && r > 0 = n - 1 - 8
          | d == NW && f > 0 && r < 7 = n - 1 + 8
          | otherwise = -1
     in if p >= 0 then p : diagonal d p else []

  -----------------------------------------------------------------------------------------------------------------------
  -- Naive route finder (ridiculous)
  -----------------------------------------------------------------------------------------------------------------------
  findRoute :: Map Square () -> Map Square () -> Square -> Square -> [Square]
  findRoute queen visited knight target = go $ knightMovesFrom target
    where go [] = []
          go (n:ns) | M.member n queen   = go ns 
                    | M.member n visited = go ns
                    | n == target        = go ns
                    | n == knight        = [target]
                    | otherwise          = 
                        case findRoute queen (M.insert target () visited) knight n of
                          [] -> go ns
                          r  -> target:r

  -----------------------------------------------------------------------------------------------------------------------
  -- Helper for setting up Dijkstra's algorithm: build the node repository.
  -----------------------------------------------------------------------------------------------------------------------
  buildGraph :: Square -> Rep Square
  buildGraph queen = let no    = notAllowed queen   -- these squares are not in the graph
                         nodes = mkNode <$> [0..63] -- the chessboard
                         r     = initNodes nodes
                      in foldl' (addNeighbours no) r nodes
    where addNeighbours l r n | M.member (nodVal n) l = r -- don't add nodes that are not in the graph
                              | otherwise = addNeis r n $ filter ( -- don't add neighbours that are not in the graph
                                              \s -> not $ M.member s l) $ knightMovesFrom (nodVal n)

