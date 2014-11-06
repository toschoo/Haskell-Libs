module Dijkstra1
where

  import Algorithms.Dijkstra

  end :: Node Int -> Bool
  end (N 9 _ _ _) = True
  end _           = False

  dst :: Node Int -> Node Int -> Integer
  dst _ _ = 1

  heu :: Node Int -> Integer
  heu _ = 0

  a,b,c,d,e,f,g,h,i :: Node Int
  a = N 1 0 [] Nothing
  b = mkNode 2
  c = mkNode 3
  d = mkNode 4
  e = mkNode 5
  f = mkNode 6
  g = mkNode 7
  h = mkNode 8
  i = mkNode 9   

  (r,is) = initNodes [a,b,c,d,e,f,g,h,i]

  r2 = addNeis r  1 [2,3]
  r3 = addNeis r2 2 [4]
  r4 = addNeis r3 4 [5]
  r5 = addNeis r4 5 [6]
  r6 = addNeis r5 6 [9]
  r7 = addNeis r6 3 [7]
  r8 = addNeis r7 7 [8]
  r9 = addNeis r8 8 [9]

  ws :: WQueue
  ws = map (\(w,n) -> W w n) $ zip (take (length is) $ repeat infinity) is

  s :: [Int]
  -- s = map nodVal $ search end dst r9 a is
  s = map nodVal $ astar end dst heu r9 a ws
