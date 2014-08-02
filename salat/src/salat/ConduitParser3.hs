{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Text/LaTeX/Base/ConduitParser.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
--
-- Conduit Parser
-------------------------------------------------------------------------------
module ConduitParser3 (conduitParser, 
                       ParseError(..), AttoparsecInput)
where 

  import           Control.Monad (unless)
  import           Control.Monad.Trans (lift)
  import           Control.Exception
  import           Data.Maybe (fromMaybe)
  import           Data.Typeable (Typeable)
  import qualified Data.Text as T
  import qualified Data.ByteString as B
  import           Data.Conduit 
  import qualified Data.Attoparsec.Types as A
  import qualified Data.Attoparsec.Text 
  import qualified Data.Attoparsec.ByteString

  data ParseError = ParseError {
      errorContexts :: [String],
      errorMessage  :: String}
      | DivergentParser
    deriving (Show, Typeable)

  instance Exception ParseError

  class AttoparsecInput a where
    parseA   :: A.Parser a b -> a -> A.IResult a b
    feedA    :: A.IResult a b -> a -> A.IResult a b
    empty    :: a
    isNull   :: a -> Bool
    notEmpty :: [a] -> [a]
    take'    :: Int -> a -> a
    length'  :: a -> Int

  instance AttoparsecInput B.ByteString where
    parseA = Data.Attoparsec.ByteString.parse
    feedA = Data.Attoparsec.ByteString.feed
    empty = B.empty
    isNull = B.null
    notEmpty = filter (not . B.null)
    take' = B.take
    length' = B.length

  instance AttoparsecInput T.Text where
    parseA = Data.Attoparsec.Text.parse
    feedA = Data.Attoparsec.Text.feed
    empty = T.empty
    isNull = T.null
    notEmpty = filter (not . T.null)
    take' = T.take
    length' = T.length
  
  -------------------------------------------------------------------------
  -- | Parser to Conduit;
  -------------------------------------------------------------------------
  conduitParser :: (MonadResource m, AttoparsecInput i) => 
                   A.Parser i o -> Pipe i i o () m ()
  conduitParser p = awaitForever (go (parseA p))
    where go p' i = case p' i of
                      A.Done t r      -> do yield r
                                            unless (isNull t) $ leftover t
                      x@(A.Partial _) -> await >>= go (feedA x) . fromMaybe empty
                      A.Fail _ c m    -> lift (monadThrow $ ParseError c m)

