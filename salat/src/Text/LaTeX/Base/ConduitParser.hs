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
module Text.LaTeX.Base.ConduitParser (conduitParser)
where 

  import           Text.LaTeX.Base.Parser (latexAtOnce)
  import           Text.LaTeX.Base.Syntax
  import qualified Data.Text as T
  import           Data.Text (Text)
  import           Data.Conduit (MonadResource, Conduit) 
  import qualified Data.Conduit.Util   as CU
  import           Data.Attoparsec.Text (parse, feed, Parser, 
                                         Result, IResult(..))

  -------------------------------------------------------------------------
  -- | Parser to Conduit;
  --   note that we call 'latexAtOnce' on any remainder at the end
  -------------------------------------------------------------------------
  conduitParser :: MonadResource m => Parser LaTeX -> Conduit Text m LaTeX
  conduitParser p = CU.conduitState Nothing push close
    where push Nothing              i = go parser i 
          push (Just   (Done t  _)) i = go parser (t <> i)
          push (Just x@(Partial _)) i = go (feed x) i
          push (Just f@(Fail{}   )) _ = fail $ errDesc f
          close (Just r) = case r of
                             Done t _ | T.null t  -> return []
                                      | otherwise -> finalise t
                             Fail _ _ e     -> error e 
                             x@(Partial  _) -> case feed x T.empty of
                                                 Done t a -> do
                                                   [b] <- finalise t
                                                   return [a <> b]
                                                 _        -> 
                                                   fail "Divergent Parser"
          close Nothing = return []
          parser        = parse p
          finalise n    = case latexAtOnce n of
                            Right r -> return [r]
                            Left  e -> fail e
          go p' i = case p' i of
                      f@(Fail{}    ) -> fail $ errDesc f
                      d@(Done _ r  ) -> return $ CU.StateProducing (Just d) [r]
                      x@(Partial _ ) | T.null i  -> 
                                        return $ CU.StateProducing (Just x) []
                                     | otherwise -> go (feed x) T.empty 

  errDesc :: Result x -> String
  errDesc (Fail t cs e) = e ++ " (" ++ show cs ++ ") - " ++ T.unpack t
  errDesc _ = ""
