module Main (main)
where

  import Algorithms.Dijkstra

  end :: Node Int -> Bool
  end (N 9 _ _ _ _) = True
  end _             = False

  dst :: Node Int -> Node Int -> Integer
  dst _ _ = 1

  heu :: Node Int -> Integer
  heu _ = 0

  a,b,c,d,e,f,g,h,i :: Node Int
  a = N 1 0 [] False Nothing
  b = mkNode 2
  c = mkNode 3
  d = mkNode 4
  e = mkNode 5
  f = mkNode 6
  g = mkNode 7
  h = mkNode 8
  i = mkNode 9   

  buildRep :: Rep Int
  buildRep = let r  = initNodes [a,b,c,d,e,f,g,h,i]
                 r2 = addNeis r  a [2,3]
                 r3 = addNeis r2 b [4]
                 r4 = addNeis r3 c [5]
                 r5 = addNeis r4 d [6]
                 r6 = addNeis r5 e [9]
                 r7 = addNeis r6 f [7]
                 r8 = addNeis r7 g [8]
              in addNeis r8 h [9]

  main :: IO ()
  main = do
    let r = buildRep
    print (map nodVal $ astar end dst heu r 1)
