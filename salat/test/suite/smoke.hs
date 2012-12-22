module Main 
where

  import Text.LaTeX.Base.Syntax
  import Text.LaTeX.Base.Parser
  import Text.LaTeX.Base.Render

  import Test.QuickCheck

  import qualified Data.ByteString      as B 
  import qualified Data.ByteString.UTF8 as U 
  import           Data.Text.Encoding
  import           Data.Attoparsec.Text (parse, IResult(..))

  import           Data.Char (toUpper)
  import qualified Data.Text as T
  import           Data.Text (Text)

  import           Control.Applicative ((<$>))
  import           System.IO
  import           System.Environment

  main :: IO ()
  main = 
    withBinaryFile "test/tex/out/jeopard.tex" ReadMode $ \h -> do
      s <- decodeUtf8 <$> B.hGetContents h
      case latexAtOnce s of
        Left  e -> putStrLn $ "Error: " ++ e
        Right r -> print r
