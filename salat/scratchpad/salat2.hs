module Main
where

  import           Parser -- Text.LaTeX.Base.Parser
  import           System.Environment
  import           System.Exit
  import           System.IO 
  import qualified Data.Text.Encoding as E
  import qualified Data.ByteString    as B
  import           Control.Applicative ((<$>))

  -- main = getContents >>= putStrLn -- . tex2rtf
  -- main = putStrLn $ tex2tex doc
  main = do
    -- putStrLn doc
    os <- getArgs
    case os of
      [f] -> withBinaryFile f ReadMode $ \h -> do
               s <- E.decodeUtf8 <$> B.hGetContents h 
               print (latexAtOnce s)
               
      _   -> do putStrLn "Input file!"
                exitFailure
