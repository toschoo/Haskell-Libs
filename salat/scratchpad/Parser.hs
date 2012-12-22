-------------------------------------------------------------------------------
-- |
-- Module     : Text/LaTeX/Parser.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
--
-- Stomp Parser based on Attoparsec
-------------------------------------------------------------------------------
module Parser (
                        latexParser,
                        latexBlockParser,
                        latexAtOnce,
                        startParsing,
                        continueParsing,
                        latexDocParser,
                        isMainDoc
                      )
where

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

  ------------------------------------------------------------------------
  -- | Starts parsing with Attoparsec 'parse'.
  --   May fail, conclude or return a partial result.
  ------------------------------------------------------------------------
  startParsing :: Text -> Either String (Result LaTeX)
  startParsing m = case parse latexParser m  of
                        Fail _ _ e -> Left e
                        r          -> Right r

  ------------------------------------------------------------------------
  -- | Continues parsing with Attoparsec 'feed'.
  --   May fail, conclude or return a partial result.
  ------------------------------------------------------------------------
  continueParsing :: Result LaTeX -> Text -> Either String (Result LaTeX)
  continueParsing r m = case feed r m of
                          Fail _ _ e -> Left e
                          r'         -> Right r'

  ------------------------------------------------------------------------
  -- | Parses a ByteString at once with Attoparsec 'parseOnly'.
  --   May fail or conclude.
  ------------------------------------------------------------------------
  latexAtOnce :: Text -> Either String LaTeX
  latexAtOnce t = 
    case parse latexParser t of
      Fail _ _ e     -> Left e
      Done _ r       -> Right r
      rx@(Partial _) -> case feed rx T.empty of
                          Fail _ _ e -> Left e
                          Partial _  -> Left "incomplete input"
                          Done _ r   -> Right r

  ------------------------------------------------------------------------
  -- | The LaTeX Parser
  ------------------------------------------------------------------------
  latexParser :: Parser LaTeX
  latexParser = blocks 

  ------------------------------------------------------------------------
  -- | Parses on block of LaTeX (without sequencing)
  ------------------------------------------------------------------------
  latexBlockParser :: Parser LaTeX
  latexBlockParser = block

  ------------------------------------------------------------------------
  -- | Parses until the /document/ envionment
  ------------------------------------------------------------------------
  latexDocParser :: Parser LaTeX
  latexDocParser = blockTillDoc

  ------------------------------------------------------------------------
  -- Blocks
  ------------------------------------------------------------------------
  blocks :: Parser LaTeX
  blocks = mconcat <$> block `manyTill` endOfInput 

  blockTillDoc :: Parser LaTeX
  blockTillDoc  = do
    b <- block
    if isMainDoc b then return  b
                   else mappend b <$> blockTillDoc

  isMainDoc :: LaTeX -> Bool
  isMainDoc b = case b of
                  TeXEnv "document" _ _ -> True
                  _                     -> False

  endOfDoc :: Parser LaTeX
  endOfDoc = endOfInput >> return TeXEmpty

  ------------------------------------------------------------------------
  -- Block
  ------------------------------------------------------------------------
  block :: Parser LaTeX
  block = choice [dolMath, comment, environment, command, text]
    
  ------------------------------------------------------------------------
  -- Text
  ------------------------------------------------------------------------
  text :: Parser LaTeX
  text = TeXRaw <$> A.takeTill (`elem` [dol, per, bsl, oBr])

  ------------------------------------------------------------------------
  -- Environment
  ------------------------------------------------------------------------
  environment :: Parser LaTeX
  environment = choice [anonym, env]

  anonym :: Parser LaTeX
  anonym = char oBr >>= \_ -> do
             b <- TeXBraces <$> blocksTill 0 eBr T.empty
             _ <- char eBr
             return b

  env :: Parser LaTeX
  env = do
    n  <- envName begin
    if n `elem` [mathEnv, displayEnv, eqEnv]
      then mathEnvironment n
      else do
        as <- cmdArgs
        b  <- envBody 0 n T.empty
        return $ TeXEnv (T.unpack n) as b

  envName :: Text -> Parser Text
  envName k = do
    _ <- string k
    _ <- char oBr
    n <- A.takeTill (== eBr)
    _ <- char eBr
    return n

  envBody :: Int -> Text -> Text -> Parser LaTeX
  envBody i n t = do
    x  <- T.append t <$> A.takeTill (== bsl)
    c  <- try (string end <|> return xxx)

    -- not end ------------------------------------------------------
    if c == xxx
      then do
        c' <- try (string begin <|> return xxx)
        if c' == xxx
          then do _ <- char bsl 
                  envBody i n (x `T.snoc` bsl)
          else do
            _ <- char oBr
            b <- A.takeTill (== eBr)
            _  <- char eBr
            let i' = if b /= n then i else i+1
            envBody i' n 
                    (x `T.append` begin `T.append` 
                        (oBr `T.cons` b `T.snoc` eBr)) 

    -- end -------------------------------------------------------------
      else do
         _  <- char oBr
         e  <- A.takeTill (== eBr) 
         _  <- char eBr
         if e /= n || i > 0
           then let i' = if e /= n then i else i-1
                 in envBody i' n (x `T.append` end `T.append` 
                                    (oBr `T.cons` e `T.snoc` eBr)) 
           else if i < 0 then fail ("Environment " ++ T.unpack e ++ " not opened")
                         else reparse x

  ------------------------------------------------------------------------
  -- Command
  ------------------------------------------------------------------------
  command :: Parser LaTeX
  command = do
    _  <- char bsl
    x  <- peekChar
    if isSpecial x
      then special
      else do
        c  <- A.takeTill endCmd
        as <- cmdArgs
        if null as
          then return $ TeXCommS (T.unpack c)
          else return $ TeXComm  (T.unpack c) as

  ------------------------------------------------------------------------
  -- Command Arguments
  ------------------------------------------------------------------------
  cmdArgs :: Parser [TeXArg]
  cmdArgs = reverse <$> cmdArgs' []

  cmdArgs' :: [TeXArg] -> Parser [TeXArg]
  cmdArgs' as = do
    x <- try (satisfy moreArgs <|> return ' ')
    if x == ' ' 
      then return as
      else do
        let e = if x == oSq then eSq else eBr
        b <- blocksTill 0 e T.empty
        _ <- char e
        if isEmpty b
          then cmdArgs' as
          else if x == oBr 
                 then cmdArgs' $ FixArg b : as
                 else cmdArgs' $ OptArg b : as
    where moreArgs w = w `elem` [oBr, oSq]

  blocksTill :: Int -> Char -> Text -> Parser LaTeX
  blocksTill i e t = do
    let o = if e == eSq then oSq else oBr
    b   <- T.append t <$> A.takeTill (`elem` [e, o])
    mbX <- peekChar
    case mbX of 
      Nothing -> fail $ "end of input reached searching for '" ++ [e] ++ "'"
      Just x  | x == e && i == 0 -> reparse b
              | x == o           -> char o >>= \_ -> 
                                      blocksTill (i+1) e (b `T.snoc` o)
              | otherwise        -> char e >>= \_ -> 
                                      blocksTill (i-1) e (b `T.snoc` e) 

  reparse :: Text -> Parser LaTeX
  reparse t = let eiR = latexAtOnce t
               in case eiR of 
                    Left  r -> fail   r
                    Right r -> return r

  special :: Parser LaTeX
  special = do
    x <- anyChar
    case x of
      '('  -> math Parentheses endPa
      '['  -> math Square      endSq
      '\\' -> do
        mbY <- peekChar 
        case mbY of
          Nothing  -> return (TeXLineBreak (-1) "" False)
          Just y | y == oSq  -> linebreak False
                 | y == str  -> do
                     _ <- char str
                     mbZ <- peekChar 
                     case mbZ of
                       Nothing -> return (TeXLineBreak (-1) "" True)
                       Just z  | z == oSq  -> linebreak True
                               | otherwise -> return (TeXLineBreak (-1) "" True)
                 | otherwise -> return (TeXLineBreak (-1) "" False)
      _   -> fail $ "Don't know what to do with this: " ++ [x]

  linebreak :: Bool -> Parser LaTeX
  linebreak t = do
    _ <- char oSq
    n <- T.unpack <$> A.takeTill (not . isNumerical)
    u <- T.unpack <$> A.takeTill (== eSq)
    _ <- char eSq
    if null n then  fail $ "NaN in linebreak: " ++ n
      else return $ TeXLineBreak (read n) u t

  isNumerical :: Char -> Bool
  isNumerical c = isDigit c || (c == '.') || (c == '-')

  ------------------------------------------------------------------------
  -- Math
  ------------------------------------------------------------------------
  dolMath :: Parser LaTeX
  dolMath = do
    _ <- char dol 
    f <- A.takeTill (== dol) 
    _ <- char dol
    parseMath Dollar f 

  math :: MathType -> Text -> Parser LaTeX
  math t eMath = getTillEMath eMath T.empty >>= parseMath t

  mathEnvironment :: Text -> Parser LaTeX
  mathEnvironment e = 
    let eMath = end `T.snoc` '{' `T.append` e `T.snoc` '}'
        mType = name2MathType e
     in math mType eMath

  getTillEMath :: Text -> Text -> Parser Text
  getTillEMath eMath t = do
    f <- T.append t <$> A.takeTill (== bsl)
    x <- try (string eMath <|> getTillEMath eMath f)
    if x == eMath then return f else return x

  parseMath :: MathType -> Text -> Parser LaTeX
  parseMath t f = case parseFormula (T.unpack f) of
                    Left  e -> fail e
                    Right x -> return $ TeXMathX t x

  ------------------------------------------------------------------------
  -- Comment 
  ------------------------------------------------------------------------
  comment :: Parser LaTeX
  comment = do
    _  <- char per
    c  <- A.takeTill (== '\n')
    _  <- char '\n'
    return $ TeXComment c

  isEmpty :: LaTeX -> Bool
  isEmpty l = case l of
                TeXEmpty -> True
                _        -> False

  isSpecial :: Maybe Char -> Bool
  isSpecial mbX = case mbX of
                    Nothing -> False 
                    Just x  -> x `elem` [bsl, oSq, oPa]

  name2MathType :: Text -> MathType
  name2MathType n = 
    case T.unpack n of
      "math"        -> MathEnv
      "displaymath" -> DispEnv
      "equation"    -> EqEnv
      _             -> Dollar

  begin, end, xxx :: Text
  begin = T.pack "\\begin"
  end   = T.pack "\\end"
  xxx   = T.pack " "

  mathEnv    = T.pack "math"
  displayEnv = T.pack "displaymath"
  eqEnv      = T.pack "equation"

  endCmd :: Char -> Bool
  endCmd = flip elem symbols

  nul, eol, spc :: Char
  nul  = '\0'
  eol  = '\n' 
  spc  = ' '

  oBr  = '{'
  eBr  = '}'
  oSq  = '['
  eSq  = ']'
  oPa  = '('
  ePa  = ')'
  bsl  = '\\'
  dol  = '$'
  per  = '%'
  str  = '*'
  eom  = chr 25

  endPa = T.pack "\\)"
  endSq = T.pack "\\]"

  symbols :: String
  symbols = [nul, eol, spc, oBr, eBr, eSq, oSq, oPa, ePa, bsl, dol, per]
  
