import Test.Hspec

import Data.Functor ((<$>))

import Algorithms.Dijkstra

main :: IO ()
main = hspec $ do
  describe "Dijkstra" $ do

    it "finds the shortest path" $ do
      let r = buildRepSimple
      nodVal <$> astar end dst heu r 1 `shouldBe` [1,3,5,9]

    it "finds the shortest path the other way round" $ do
      let r = buildRepSimple2
      nodVal <$> astar end dst heu r 1 `shouldBe` [1,3,5,9]

    it "finds the shortest path with nodes reversed" $ do
      let r = buildRepSimpleR
      nodVal <$> astar end dst heu r 1 `shouldBe` [1,3,5,9]

    it "can manage cycles" $ do
      let r = buildRepWithCycle
      nodVal <$> astar end dst heu r 1 `shouldBe` [1,2,4,6,7,8,9]

    it "can manage cycles the other way round" $ do
      let r = buildRepWithCycle2
      nodVal <$> astar end dst heu r 1 `shouldBe` [1,2,4,6,7,8,9]

    it "terminates without path" $ do
      let r = buildRepWithoutPath
      nodVal <$> astar end dst heu r 1 `shouldBe` [9]

    it "terminates without path more subtle" $ do
      let r = buildRepWithoutPathSubtle
      nodVal <$> astar end dst heu r 1 `shouldBe` [9]

    it "terminates when not finding target" $ do
      let r = buildRepSimple
      nodVal <$> astar noend dst heu r 1 `shouldBe` []


makeNodes :: [Node Int]
makeNodes = [ N 1 0 [] False Nothing
            , mkNode 2
            , mkNode 3
            , mkNode 4
            , mkNode 5
            , mkNode 6
            , mkNode 7
            , mkNode 8
            , mkNode 9
            ]

buildRepSimple :: Rep Int
buildRepSimple = case makeNodes of
                   [a,b,c,d,e,f,g,h,i] ->
                     let r1 = initNodes $ [a,b,c,d,e,f,g,h,i]
                         r2 = addNeis r1 a [2,3]
                         r3 = addNeis r2 b [4]
                         r4 = addNeis r3 c [5]
                         r5 = addNeis r4 d [6]
                         r6 = addNeis r5 e [9]
                         r7 = addNeis r6 f [7]
                         r8 = addNeis r7 g [8]
                      in addNeis r8 h [9]

buildRepSimple2 :: Rep Int
buildRepSimple2 = case makeNodes of
                   [a,b,c,d,e,f,g,h,i] ->
                     let r1 = initNodes [a,b,c,d,e,f,g,h,i]
                         r2 = addNeis r1 a [3,2]
                         r3 = addNeis r2 b [4]
                         r4 = addNeis r3 c [5]
                         r5 = addNeis r4 d [6]
                         r6 = addNeis r5 e [9]
                         r7 = addNeis r6 f [7]
                         r8 = addNeis r7 g [8]
                      in addNeis r8 h [9]

buildRepSimpleR :: Rep Int
buildRepSimpleR = case makeNodes of
                   [a,b,c,d,e,f,g,h,i] ->
                     let r1 = initNodes $ reverse [a,b,c,d,e,f,g,h,i]
                         r2 = addNeis r1 a [2,3]
                         r3 = addNeis r2 b [4]
                         r4 = addNeis r3 c [5]
                         r5 = addNeis r4 d [6]
                         r6 = addNeis r5 e [9]
                         r7 = addNeis r6 f [7]
                         r8 = addNeis r7 g [8]
                      in addNeis r8 h [9]

buildRepWithCycle :: Rep Int
buildRepWithCycle = case makeNodes of
                      [a,b,c,d,e,f,g,h,i] ->
                         let r1 = initNodes [a,b,c,d,e,f,g,h,i]
                             r2 = addNeis r1 a [2,3]
                             r3 = addNeis r2 b [4]
                             r4 = addNeis r3 c [5]
                             r5 = addNeis r4 d [6]
                             r6 = addNeis r5 e [1]
                             r7 = addNeis r6 f [7]
                             r8 = addNeis r7 g [8]
                          in addNeis r8 h [9]

buildRepWithCycle2 :: Rep Int
buildRepWithCycle2 = case makeNodes of
                       [a,b,c,d,e,f,g,h,i] ->
                          let r1 = initNodes [a,b,c,d,e,f,g,h,i]
                              r2 = addNeis r1 a [3,2]
                              r3 = addNeis r2 b [4]
                              r4 = addNeis r3 c [5]
                              r5 = addNeis r4 d [6]
                              r6 = addNeis r5 e [1]
                              r7 = addNeis r6 f [7]
                              r8 = addNeis r7 g [8]
                           in addNeis r8 h [9]

buildRepWithoutPath :: Rep Int
buildRepWithoutPath = initNodes makeNodes

buildRepWithoutPathSubtle :: Rep Int
buildRepWithoutPathSubtle = case makeNodes of
                              [a,b,c,d,e,f,g,h,i] ->
                                 let r1 = initNodes [a,b,c,d,e,f,g,h,i]
                                     r2 = addNeis r1 a [3,2]
                                     r3 = addNeis r2 b [4]
                                     r4 = addNeis r3 c [5]
                                     r5 = addNeis r4 d [6]
                                     r6 = addNeis r5 e [1]
                                     r7 = addNeis r6 f [7]
                                     r8 = addNeis r7 g [8]
                                  in r8

end :: Node Int -> Bool
end (N 9 _ _ _ _) = True
end _             = False

noend :: Node Int -> Bool
noend _ = False

dst :: Node Int -> Node Int -> Integer
dst _ _ = 1

heu :: Node Int -> Integer
heu _ = 0


