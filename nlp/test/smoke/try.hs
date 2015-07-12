module Main
where

  import qualified Data.Text.IO as TIO
  import           System.Environment
  
  import           NLP.RAKE.Text

  kwFromLim :: Double -> [WordScore] -> [WordScore]
  kwFromLim d = filter (limit d)

  kwFromAvg :: [WordScore] -> [WordScore]
  kwFromAvg ws = kwFromLim d ws
    where wsum = sum $ map snd ws
          wlen = fromIntegral $ length ws
          d    = wsum / wlen

  kwFromMedian :: [WordScore] -> [WordScore]
  kwFromMedian ws = kwFromLim d ws
    where wlen = length ws
          d = let (l,m) | even wlen = (2,take 2 $ drop (wlen `div` 2 - 1) ws)
                        | otherwise = (1,take 1 $ drop ((wlen-1) `div` 2) ws)
               in (sum $ map snd m) / l
  
  limit :: Double -> WordScore -> Bool
  limit d (_,s) = d <= s

  tstText1 :: String
  tstText1 = "Compatibility of systems of linear constraints over the set of natural numbers. Criteria of compatibility of a system of linear Diophantine equations, strict inequations, and nonstrict inequations are considered. Upper bounds for components of a minimal set of solutions and algorithms of construction of minimal generating sets of solutions for all types of systems are given. These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions can be used in solving all the considered types of systems and systems of mixed types."

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [f] -> test f
      _   -> error "I need a file name"

  test :: FilePath -> IO ()
  test f = do
    t <- TIO.readFile f
    print (kwFromAvg $ keywords t)

  



