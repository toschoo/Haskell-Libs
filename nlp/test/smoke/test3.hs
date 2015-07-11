module Main
where
  
  import           NLP.RAKE.Stopwords
  import           NLP.RAKE.Text
  import           System.Environment
  import qualified Data.Text.IO as TIO

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [f] -> test f
      _   -> error "I need a file..."

  test :: FilePath -> IO ()
  test = do
    t <- TIO.readFile f
    print (filter (\(_,s) -> s > 1.0) $ candidates m $ pSplitter t) 

