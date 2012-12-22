module Main
where

  import System.Environment
  import System.FilePath
  import Control.Applicative ((<$>))
  import qualified Data.Text as T

  import Text.LaTeX.Base.Parser
  import Text.LaTeX.Base.Render

  main :: IO ()
  main = do
    os <- getArgs
    case os of 
      [f] -> if takeExtension f /= ""
               then error "file name without extension, please" 
               else doTest f
      _   -> error "I need a file name"

  doTest :: FilePath -> IO ()
  doTest f = do
    s <- readFile (replaceExtension f "tex") 
    putStrLn $ T.unpack (T.pack s)
    case parseLaTeX (T.pack s) of
      Left  e -> putStrLn "Error!" -- $ show e
      Right p -> do
       putStrLn "hello!" -- $ show p
       --  renderFile (replaceExtension f "tex2") p

