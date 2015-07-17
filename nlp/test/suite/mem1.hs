module Main
where

  import qualified Data.Text.IO as TIO
  import           NLP.RAKE.Text
  import           System.Environment
  import           Control.Applicative ((<$>))

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [f] -> dostuff f
      _   -> error "I need a file name!"

  dostuff :: FilePath -> IO ()
  dostuff f = loop 1
    where loop 0 = putStrLn "ready"
          loop _ = do
            kw <- keywords <$> TIO.readFile f
            loop (sum (map snd $ kw))
    
    
