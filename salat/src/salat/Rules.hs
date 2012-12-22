module Rules (versify, include)
where

  import           Text.LaTeX.Base.Syntax
  import           Data.Text(Text, snoc, append)
  import qualified Data.Text as T

  --------------------------------------------------------------------
  -- | Transform plain text to poem 
  --------------------------------------------------------------------
  versify :: Text -> Text
  versify = go . T.lines
    where go []                   = T.empty
          go [x]                  = x
          go (x:y:ys) | T.null x  = go (y:ys)
                      | T.head x == '\\' = (x |> ' ') <> go (y:ys)
                      | T.null y  = (x >|< stanza)    <> go    ys 
                      | otherwise = (x >|< verse )    <> go (y:ys)

  --------------------------------------------------------------------
  -- | Replace \"inputbase\"
  --------------------------------------------------------------------
  include :: String -> LaTeX -> LaTeX
  include path (TeXComm "inputbase" args) = TeXComm "input" $ 
                                              replaceBase path args
  include path (TeXEnv s a b) = TeXEnv s a $ include path b
  include path (TeXMath    b) = TeXMath    $ include path b
  include path (TeXBraces  b) = TeXBraces  $ include path b
  include path (TeXSeq   x y) = TeXSeq (include path x) (include path y)
  include _    x              = x 

  --------------------------------------------------------------------
  -- Add texalog path to local path
  --------------------------------------------------------------------
  replaceBase :: String -> [TeXArg] -> [TeXArg]
  replaceBase _ [] = []
  replaceBase path (x:xs) = 
    case x of
      FixArg (TeXRaw t) -> FixArg (TeXRaw ((T.pack path |> '/') <> t)) : xs
      y -> y : replaceBase path xs

  --------------------------------------------------------------------
  -- Helpers
  --------------------------------------------------------------------
  stanza :: Text
  stanza = T.pack "\\\\[10pt]\n"

  verse :: Text
  verse = T.pack "\\\\\n"

  infixr 6 |>
  (|>) :: Text -> Char -> Text 
  (|>) = snoc

  infixr 5 >|<
  (>|<) :: Text -> Text -> Text 
  (>|<) = append
