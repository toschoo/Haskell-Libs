-------------------------------------------------------------------------------
-- |
-- Module     : Network.Api.Arxiv.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
-------------------------------------------------------------------------------
module {- Network.Api. -} Arxiv (
               -- * Request 
               baseUrl, apiUrl, apiQuery,
               Field(..), Expression(..),
               (/+/), (/-/), (/!/),
               Query(..), mkQuery, parseQuery, 
               qry2str, items2str,
               itemControl,
               phrase, url,

               -- * Response
               Author(..), Link(..), Category(..),
               totalResults, startIndex, itemsPerPage,
               getEntry, forEachEntry, forEachEntryM, forEachEntryM_,
               getId, getUpdated, getPublished,
               getTitle, getSummary,
               getComment, getJournal, getDoi,
               getLinks, getPdfLink, getPdf,
               getCategories,
               getAuthors, getAuthorNames)
where

  import Text.HTML.TagSoup
  import Data.Char (isDigit)
  import Data.Maybe (catMaybes, fromMaybe)
  import Data.List  (find)
  import Text.Parsec

  baseUrl :: String
  baseUrl =  "arxiv.org"

  apiUrl :: String
  apiUrl =  "http://export.arxiv.org/api/"

  apiQuery :: String
  apiQuery =  "query?search_query="

  -- query
  data Field = Ti  String
             | Au  String
             | Ab  String
             | Com String
             | Jr  String
             | Cat String
             | Rn  String
             | Id  String
             | All String
    deriving (Eq, Show)

  field2str :: Field -> String
  field2str (Ti  s) = "ti:"  ++ s
  field2str (Au  s) = "au:"  ++ s
  field2str (Ab  s) = "abs:" ++ s
  field2str (Com s) = "co:"  ++ s
  field2str (Jr  s) = "jr:"  ++ s
  field2str (Cat s) = "cat:" ++ s
  field2str (Rn  s) = "rn:"  ++ s
  field2str (Id  s) = "id:"  ++ s
  field2str (All s) = "all:" ++ s

  data Expression = Exp Field
                   | And    Expression Expression
                   | Or     Expression Expression
                   | AndNot Expression Expression
    deriving (Eq, Show)

  -- infixes And, Or, AndNot
  -- Au "Knuth" `And` (Or (Ti (phrase "The art of computer programming"))
  --                      (Ti (phrase "Concrete Mathematics")))
  -- Au "Knuth" /+/ ((Ti $ phrase "...") /-/ (Ti $ phrase "..."))

  infix /+/
  (/+/) :: Expression -> Expression -> Expression
  (/+/) = And

  infix /-/
  (/-/) :: Expression -> Expression -> Expression
  (/-/) = Or

  infix /!/
  (/!/) :: Expression -> Expression -> Expression
  (/!/) = AndNot

  exp2str, innerExp2str :: Expression -> String
  exp2str (Exp f)        = field2str f
  exp2str (And e1 e2)    = innerExp2str e1 ++ "+AND+"    ++ innerExp2str e2
  exp2str (Or  e1 e2)    = innerExp2str e1 ++ "+OR+"     ++ innerExp2str e2
  exp2str (AndNot e1 e2) = innerExp2str e1 ++ "+AndNot+" ++ innerExp2str e2
  innerExp2str (Exp f)   = exp2str (Exp f)
  innerExp2str e         = "%28" ++ exp2str e ++ "%29"

  data Query = Query {
                 qExp   :: Expression,
                 qStart :: Int,
                 qItems :: Int}
    deriving (Eq, Show)

  qry2str :: Query -> String
  qry2str q = exp2str (qExp q) ++ items2str q

  items2str :: Query -> String
  items2str q = itemControl (qStart q) (qItems q) 

  itemControl :: Int -> Int -> String
  itemControl s m = "&amp;start="      ++ show s ++
                    "&amp;maxResults=" ++ show m

  parseQuery :: String -> Either String Expression
  parseQuery s = case parse expression "" s of
                   Left  e -> Left $ show e
                   Right e -> Right e

  phrase :: String -> String
  phrase "" = ""
  phrase s  = "%22" ++ url s ++ "%22"

  url :: String -> String
  url "" = ""
  url (c:cs) | c == ' '  = '+' : url cs
             | otherwise = c   : url cs

  mkQuery :: Query -> String
  mkQuery q   = apiUrl ++ apiQuery ++ x ++ itm
    where x   = qry2str   q
          itm = items2str q

  -- result

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

  getId :: [Tag String] -> Maybe String
  getId = getString "id"

  getUpdated :: [Tag String] -> Maybe String
  getUpdated = getString "updated"

  getPublished :: [Tag String] -> Maybe String
  getPublished = getString "published"

  getTitle :: [Tag String] -> Maybe String
  getTitle = getString "title"

  getSummary :: [Tag String] -> Maybe String
  getSummary = getString "summary"

  getComment :: [Tag String] -> Maybe String
  getComment = getString "arxiv:comment"

  getJournal :: [Tag String] -> Maybe String
  getJournal = getString "arxiv:journal_ref"

  getDoi :: [Tag String] -> Maybe String
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
    where mkCat c = Category {
                      catTerm   = fromMaybe "" $ getAt "term" c,
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
  getAuthorNames soup = catMaybes $ go soup
    where go s = case element "author" s of
                   (a,[]) -> [getString "name" a]
                   ([],_) -> []
                   (a,r)  -> getString "name" a : go r

  getAt :: String -> Tag String -> Maybe String
  getAt a (TagOpen _ as) = lookup a as 
  getAt _ _              = Nothing

  getString :: String -> [Tag String] -> Maybe String
  getString n soup = let (i,_) = element n soup 
                      in if null i then Nothing else Just (findTxt i)

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
  expression :: Parsec String () Expression
  expression = undefined
