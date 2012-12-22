module Main where

  import           Data.Attoparsec.Text hiding (take, takeWhile, takeTill)
  import qualified Data.Attoparsec.Text as A   (takeWhile, takeTill)
  import           Data.Char (isDigit, chr)
  import           Data.Monoid
  import           Data.Text (Text(..))
  import qualified Data.Text as T 
  import           Data.List (foldl')

  import           Control.Applicative ((<|>), (<$>))
  import           Control.Monad.Trans (liftIO)

  import           Text.LaTeX.Base.Syntax
  import           Text.TeXMath.Types
  import           Text.TeXMath.Parser

  takeTillOrEnd :: (Char -> Bool) -> Parser Text
  takeTillOrEnd p = (T.pack . reverse) <$> takeTillOrEndX [] p

  takeTillOrEndX :: String -> (Char -> Bool) -> Parser String
  takeTillOrEndX r p = do
    mbC <- peekChar
    case mbC of
      Nothing -> return r
      Just c  -> if p c then char c >>= \_ -> return r
                        else do _ <- char c
                                takeTillOrEndX (c : r) p

  dotParser :: Parser [Text]
  dotParser = manyTill (takeTillOrEnd (== '.')) endOfInput
  
  main = do 
    let r1 = parse dotParser (T.pack "eins.zwei.drei")
    let r2 = feed r1 T.empty
    print r2
     

