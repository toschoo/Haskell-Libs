-------------------------------------------------------------------------------
-- |
-- Module     : Network.Api.Arxiv.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
-------------------------------------------------------------------------------
module Network.Api.Arxiv (
               -- * Request 
               baseUrl, apiUrl, apiQuery,
               Field(..), Expression(..),
               (/*/), (/+/), (/-/),
               Query(..), mkQuery, parseQuery, 
               exp2str, qry2str, items2str,
               itemControl,

               -- * Response
               Author(..), Link(..), Category(..),
               totalResults, startIndex, itemsPerPage,
               getEntry, forEachEntry, forEachEntryM, forEachEntryM_,
               getId, getUpdated, getPublished, getYear,
               getTitle, getSummary,
               getComment, getJournal, getDoi,
               getLinks, getPdfLink, getPdf,
               getCategories, getPrimaryCategory,
               getAuthors, getAuthorNames)
where

  import Text.HTML.TagSoup
  import Text.Parsec
  import Data.Char (isDigit)
  import Data.Maybe (fromMaybe)
  import Data.List  (find, intercalate)
  import Control.Applicative ((<$>))
  import Control.Monad (void)
  import Debug.Trace (trace)

  ------------------------------------------------------------------------ 
  -- | The Arxiv base URL \"arxiv.org\"
  ------------------------------------------------------------------------ 
  baseUrl :: String
  baseUrl =  "arxiv.org"

  ------------------------------------------------------------------------ 
  -- | The Arxiv API URL \"export.arxiv.org/api\"
  ------------------------------------------------------------------------ 
  apiUrl :: String
  apiUrl =  "http://export.arxiv.org/api/"

  ------------------------------------------------------------------------ 
  -- | The query string (\"query?search_query=\")
  --   TODO: id_list!
  ------------------------------------------------------------------------ 
  apiQuery :: String
  apiQuery =  "query?search_query="

  ------------------------------------------------------------------------ 
  -- | Field data type;
  --   a field consist of a field identifier
  --   (author, title, etc.)
  --   and a list of search terms.
  ------------------------------------------------------------------------ 
  data Field = 
             -- | Title
             Ti    [Term]

             -- | Author
             | Au  [Term]

             -- | Abstract
             | Abs [Term]

             -- | Comment
             | Co  [Term]

             -- | Journal
             | Jr  [Term]

             -- | Category
             | Cat [Term]

             -- | Report Number
             | Rn  [Term]

             -- | Article identifier
             | Id  [Term]

             -- | Any of the above
             | All [Term]
    deriving (Eq, Show)

  ------------------------------------------------------------------------
  -- | A term is just a string
  ------------------------------------------------------------------------
  type Term = String

  ------------------------------------------------------------------------
  -- convert a field to a string
  ------------------------------------------------------------------------
  field2str :: Field -> String
  field2str (Ti  s) = "ti:"  ++ terms2str s
  field2str (Au  s) = "au:"  ++ terms2str s
  field2str (Abs s) = "abs:" ++ terms2str s
  field2str (Co  s) = "co:"  ++ terms2str s
  field2str (Jr  s) = "jr:"  ++ terms2str s
  field2str (Cat s) = "cat:" ++ terms2str s
  field2str (Rn  s) = "rn:"  ++ terms2str s
  field2str (Id  s) = "id:"  ++ terms2str s
  field2str (All s) = "all:" ++ terms2str s

  ------------------------------------------------------------------------
  -- convert a term to a string
  ------------------------------------------------------------------------
  terms2str :: [Term] -> String
  terms2str = intercalate "+" . map term2str 
    where term2str t = 
            let x = intercalate "+" (words t)
             in if '+' `elem` x then "%22" ++ x ++ "%22" else x

  ------------------------------------------------------------------------
  -- | Expression data type.
  --   An expression is either a field or a logical connection
  --   of two expressions using the basic operators 
  --   AND, OR and ANDNOT.
  ------------------------------------------------------------------------
  data Expression = 
                   -- | Just a field
                   Exp Field

                   -- | Logical \"and\"
                   | And    Expression Expression

                   -- | Logical \"or\"
                   | Or     Expression Expression

                   -- | Logical \"and . not\"
                   | AndNot Expression Expression
    deriving (Eq, Show)

  ------------------------------------------------------------------------
  -- | AND operator.
  --   The symbol was chosen because
  --   0 * 1 = 0.
  ------------------------------------------------------------------------
  infix /*/
  (/*/) :: Expression -> Expression -> Expression
  (/*/) = And

  ------------------------------------------------------------------------
  -- | OR operator.
  --   The symbol was chosen because
  --   0 + 1 = 1.
  ------------------------------------------------------------------------
  infix /+/
  (/+/) :: Expression -> Expression -> Expression
  (/+/) = Or 

  ------------------------------------------------------------------------
  -- | ANDNOT operator.
  --   The symbol was chosen because
  --   1 - 1 = 0 and 1 - 0 = 1.
  ------------------------------------------------------------------------
  infix /-/
  (/-/) :: Expression -> Expression -> Expression
  (/-/) = AndNot

  ------------------------------------------------------------------------
  -- | Create a query string from an expression.
  --   Note that we create redundant parentheses,
  --   for instance \"a AND b OR c\" will be encoded as
  --   \"a+AND+%28b+OR+c%29\".
  --   The reason is that the API specification is not clear
  --   on how expressions are parsed.
  --   The above expression could be understood as
  --   \"a AND (b OR c)\" or \"(a AND b) or c\".
  --   To avoid confusion, one should always use parentheses
  --   to group boolean expressions - even if some of these parentheses
  --   appear to be redundant under one or the other parsing strategy.
  ------------------------------------------------------------------------
  exp2str, innerExp2str :: Expression -> String
  exp2str (Exp f)        = field2str f
  exp2str (And e1 e2)    = innerExp2str e1 ++ "+AND+"    ++ innerExp2str e2
  exp2str (Or  e1 e2)    = innerExp2str e1 ++ "+OR+"     ++ innerExp2str e2
  exp2str (AndNot e1 e2) = innerExp2str e1 ++ "+ANDNOT+" ++ innerExp2str e2
  innerExp2str (Exp f)   = exp2str (Exp f)
  innerExp2str e         = "%28" ++ exp2str e ++ "%29"

  ------------------------------------------------------------------------
  -- | Query data type
  ------------------------------------------------------------------------
  data Query = Query {
                 -- | The query expression
                 qExp   :: Expression,
                 -- | The first item we want to see
                 qStart :: Int,
                 -- | The number of items we want to see
                 qItems :: Int}
    deriving (Eq, Show)

  -------------------------------------------------------------------------
  -- | Converts the query including item control to string
  -------------------------------------------------------------------------
  qry2str :: Query -> String
  qry2str q = exp2str (qExp q) ++ items2str q

  -------------------------------------------------------------------------
  -- | Converts the query including url, query expression and item control
  -------------------------------------------------------------------------
  mkQuery :: Query -> String
  mkQuery q   = apiUrl ++ apiQuery ++ x ++ itm
    where x   = qry2str   q
          itm = items2str q

  -------------------------------------------------------------------------
  -- | Converts the query to a string containing only the item control
  -------------------------------------------------------------------------
  items2str :: Query -> String
  items2str q = itemControl (qStart q) (qItems q) 

  -------------------------------------------------------------------------
  -- | Generates the item control of a query string according
  --   to first item and number of items
  -------------------------------------------------------------------------
  itemControl :: Int -> Int -> String
  itemControl s m = "&amp;start="      ++ show s ++
                    "&amp;maxResults=" ++ show m

  -------------------------------------------------------------------------
  -- | Parses an expression from a string
  --   (without item control)
  -------------------------------------------------------------------------
  parseQuery :: String -> Either String Expression
  parseQuery s = case parse expression "" s of
                   Left  e -> Left $ show e
                   Right e -> Right e

  -------------------------------------------------------------------------
  -- result
  -------------------------------------------------------------------------
  data Author = Author {
                  auName :: String,
                  auFil  :: String}
    deriving (Show, Eq)

  data Link = Link {
                lnkHref  :: String,
                lnkType  :: String, -- could be Mime
                lnkTitle :: String,
                lnkRel   :: String}
    deriving (Show, Eq)

  data Category = Category {
                    catTerm   :: String,
                    catScheme :: String}
    deriving (Show, Eq)

  totalResults :: [Tag String] -> Int
  totalResults = getN "opensearch:totalResults" 

  startIndex :: [Tag String] -> Int
  startIndex = getN "opensearch:startIndex"

  itemsPerPage :: [Tag String] -> Int
  itemsPerPage = getN "opensearch:itemsPerPage"

  getEntry :: [Tag String] -> ([Tag String],[Tag String])
  getEntry = element "entry"

  forEachEntry :: [Tag String] -> ([Tag String] -> r) -> [r]
  forEachEntry = forEach "entry"

  forEachEntryM :: Monad m =>
                   [Tag String] -> ([Tag String] -> m r) -> m [r]
  forEachEntryM = forEachM "entry"
                   
  forEachEntryM_ :: Monad m =>
                    [Tag String] -> ([Tag String] -> m ()) -> m ()
  forEachEntryM_ = forEachM_ "entry"

  getId :: [Tag String] -> String
  getId = getString "id"

  getUpdated :: [Tag String] -> String
  getUpdated = getString "updated"

  getPublished :: [Tag String] -> String
  getPublished = getString "published"

  getYear :: [Tag String] -> String
  getYear sp = case getPublished sp of
                 "" -> "s.a."
                 s  -> takeWhile (/= '-') s

  getTitle :: [Tag String] -> String
  getTitle = getString "title"

  getSummary :: [Tag String] -> String
  getSummary = getString "summary"

  getComment :: [Tag String] -> String
  getComment = getString "arxiv:comment"

  getJournal :: [Tag String] -> String
  getJournal = getString "arxiv:journal_ref"

  getDoi :: [Tag String] -> String
  getDoi = getString "arxiv:doi"

  getLinks :: [Tag String] -> [Link]
  getLinks soup = case element "link" soup of
                    ([],_)     -> []
                    (x:_,[]) -> [mkLink x]
                    (x:_,rs) -> mkLink x : getLinks rs
    where mkLink l = Link {
                       lnkHref  = fromMaybe "" $ getAt "href"  l,
                       lnkTitle = fromMaybe "" $ getAt "title" l,
                       lnkRel   = fromMaybe "" $ getAt "rel"   l,
                       lnkType  = fromMaybe "" $ getAt "type"  l}

  getPdfLink :: [Tag String] -> Maybe Link
  getPdfLink soup = case getLinks soup of
                      [] -> Nothing
                      ls -> find (\l -> lnkTitle l == "pdf") ls

  getPdf :: [Tag String] -> String
  getPdf soup = case getPdfLink soup of
                  Nothing -> ""
                  Just l  -> lnkHref l

  getCategories :: [Tag String] -> [Category]
  getCategories soup = case element "category" soup of
                         ([],_)   -> []
                         (x:_,[]) -> [mkCat x]
                         (x:_,rs) -> mkCat x : getCategories rs

  getPrimaryCategory :: [Tag String] -> Maybe Category
  getPrimaryCategory soup = case element "arxiv:primary_category" soup of
                              ([],_)  -> Nothing
                              (x:_,_) -> Just (mkCat x)

  mkCat :: Tag String -> Category
  mkCat c = Category {
              catTerm   = fromMaybe "" $ getAt "term"   c,
              catScheme = fromMaybe "" $ getAt "scheme" c}

  getAuthors :: [Tag String] -> [Author]
  getAuthors soup = case element "author" soup of
                      ([],_)     -> []
                      (xs,rs) -> mkAut xs : getAuthors rs
    where mkAut au = let nm = case element "name" au of
                                ([],_) -> ""
                                (n,_)  -> findTxt n
                         fl = case element "arxiv:affiliation" au of
                                ([],_) -> ""
                                (a,_)  -> findTxt a
                      in Author {
                           auName = nm,
                           auFil  = fl}

  getAuthorNames :: [Tag String] -> [String]
  getAuthorNames = go 
    where go s = case element "author" s of
                   ([],[]) -> []
                   (a,[])  -> [getString "name" a]
                   (a,r)   ->  getString "name" a : go r

  getAt :: String -> Tag String -> Maybe String
  getAt a (TagOpen _ as) = lookup a as 
  getAt _ _              = Nothing

  getString :: String -> [Tag String] -> String
  getString n soup = let (i,_) = element n soup 
                      in if null i then "" else findTxt i

  getN :: String -> [Tag String] -> Int
  getN key soup = case element key soup of
                    (k,_) -> case findTxt k of
                               "" -> -1
                               t  -> if all isDigit t then read t else -1

  findTxt :: [Tag String] -> String
  findTxt [] = ""
  findTxt (t:ts) = case t of
                     TagText x -> x
                     _         -> findTxt ts

  forEach :: String -> [Tag String] -> ([Tag String] -> r) -> [r]
  forEach nm soup f = case element nm soup of
                        ([],_) -> []
                        (e,rs) -> f e : forEach nm rs f

  forEachM :: Monad m => 
              String  -> [Tag String] -> ([Tag String] -> m r) -> m [r]
  forEachM nm soup f = case element nm soup of
                         ([],_) -> return []
                         (e,rs) -> do r  <- f e 
                                      rr <- forEachM nm rs f
                                      return (r:rr)

  forEachM_ :: Monad m => 
               String  -> [Tag String] -> ([Tag String] -> m ()) -> m ()
  forEachM_ nm soup f = case element nm soup of
                          ([],_) -> return ()
                          (e,rs) -> f e >> forEachM_ nm rs f 

  element :: String -> [Tag String] -> ([Tag String], [Tag String])
  element _  []     = ([],[])
  element nm (t:ts) | isTagOpenName nm t = let (r,rs) = closeEl 0 ts
                                            in (t:r,rs)
                    | otherwise          = element nm ts
    where closeEl :: Int -> [Tag String] -> ([Tag String], [Tag String])
          closeEl _ [] = ([],[])
          closeEl i (x:xs) = go i (isTagCloseName nm x) x xs
          go i b x xs | b && i == 0        = ([x],xs) 
                      | b && i >  0        = let (r,rs) = closeEl (i-1) xs 
                                              in (x:r,rs)
                      | isTagOpenName nm x = let (r,rs) = closeEl (i+1) xs 
                                              in (x:r,rs)
                      | otherwise          = let (r,rs) = closeEl i     xs 
                                              in (x:r,rs)

  -- Parser --------------------------------------------------------------
  type Parser a = Parsec String () a

  expression :: Parser Expression
  expression = try parentheses <|> fieldOperator

  fieldOperator :: Parser Expression
  fieldOperator = do
    f <- field
    c <- try (char '+') <|> return ' '
    if c == ' ' then return   f
                else opAndArg f
 
  opAndArg :: Expression -> Parser Expression
  opAndArg f = do
    o <- op
    void $ char '+'
    e <- expression
    return (o f e)

  field :: Parser Expression
  field = do 
    i  <- fieldId 
    ts <- terms
    return (Exp $ i ts)
     
  fieldId :: Parser ([Term] -> Field)
  fieldId =   try (void (string "au:" ) >> return Au)
          <|> try (void (string "ti:" ) >> return Ti)
          <|> try (void (string "abs:") >> return Abs)
          <|> try (void (string "co:" ) >> return Co)
          <|> try (void (string "cat:") >> return Cat)
          <|> try (void (string "jr:" ) >> return Jr)
          <|> try (void (string "rn:" ) >> return Rn)
          <|> try (void (string "id:" ) >> return Id)
          <|>     (void (string "all:") >> return All)

  terms :: Parser [String]
  terms = do
    t <- try quoted <|> term
    c <- try (lookAhead anyChar) <|> onEof '%' --ugly
    case c of
      '%' -> return [t]
      '+' -> do x <- isOp
                if x then return [t] 
                     else void (char c) >> (t:) <$> terms
      _   -> fail $ "unexpected symbol: '" ++ [c] ++ "'"

  isOp :: Parser Bool
  isOp =     try (void (lookAhead (string "+ANDNOT+")) >> return True)
         <|> try (void (lookAhead (string "+AND+"))    >> return True)
         <|> try (void (lookAhead (string "+OR+"))     >> return True)
         <|> return False

  quoted :: Parser String
  quoted = do
    void $ string "%22"
    intercalate "+" <$> go
    where go = do
            t <- term
            s <- try (string "%22") <|> return ""
            if not (null s) then return [t] 
                            else do c <- anyChar -- void (char '+') -- could be '%' as well
                                    let t' = if c == '+' then t 
                                                         else t ++ [c]
                                    (t':) <$> go
    
  term :: Parser String
  term = do 
    c <- try (lookAhead anyChar) <|> onEof '%'
    if c `elem` "%+"
      then return ""
      else do x <- char c
              (x:) <$> term

  onEof :: Char -> Parser Char
  onEof c = eof >> return c

  parentheses :: Parser Expression
  parentheses = do
    void $ string "%28"
    e <- expression
    void $ string "%29"
    c <- try (char '+') <|> try (lookAhead (char '%')) <|> onEof '.'
    case c of
      '+' -> opAndArg e
      _   -> return   e

  op :: Parser (Expression -> Expression -> Expression)
  op =       try (void (string "ANDNOT") >> return AndNot) 
         <|> try (void (string "OR")     >> return Or) 
         <|>     (void (string "AND")    >> return And)
      

