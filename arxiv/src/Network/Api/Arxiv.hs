-------------------------------------------------------------------------------
-- |
-- Module     : Network.Api.Arxiv.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
--
-- The ArXiv API is split in two parts:
-- Request and Response.
-- The Request part contains
-- a simple language to define queries,
-- a query parser and some helpers to navigate
-- through the results of a mutlipage query
-- (which, in fact, induces a new query).
-- 
-- The Response part contains
-- an API to access the fields of the result
-- based on TagSoup.
--
-- This library does not contain functions
-- to actually execute and manage http requests.
-- It is intended to be used with existing
-- http libraries such as http-conduit.
-- An example how to use the ArXiv library
-- with http-conduit is included in this documentation.
-------------------------------------------------------------------------------
module Network.Api.Arxiv (
               -- * Request 
               -- $RequestOv
               baseUrl, apiUrl, apiQuery,
               Field(..), Expression(..),
               (/*/), (/+/), (/-/),

               -- * Expression Example
               -- $ExpExample

               -- * Queries
               Query(..), nextPage, 
               parseQuery, preprocess, parseIds,
               mkQuery, exp2str, items2str, ids2str,
               itemControl,

               -- * Response
               -- $ResponseOv
               totalResults, startIndex, itemsPerPage,
               getEntry, forEachEntry, forEachEntryM, forEachEntryM_,
               checkForError, exhausted,
               getId, getIdUrl, getUpdated, getPublished, getYear,
               getTitle, getSummary,
               getComment, getJournal, getDoi,
               Link(..), 
               getLinks, getPdfLink, getPdf,
               Category(..),
               getCategories, getPrimaryCategory,
               Author(..), 
               getAuthors, getAuthorNames

               -- * A complete Example using http-conduit
               -- $CompleteExample
               )
where

  import           Text.HTML.TagSoup
  import           Text.Parsec
  import           Data.Char (isDigit)
  import           Data.Maybe (fromMaybe)
  import           Data.List  (find, intercalate)
  import qualified Data.List.Split as S 
  import           Control.Applicative ((<$>))
  import           Control.Monad (void)
  
  ------------------------------------------------------------------------
  -- import Debug.Trace (trace)
  ------------------------------------------------------------------------

  {- $ExpExample

     Expressions are intended to ease the construction
     of well-formed queries in application code. 
     A simple example:

     > let au = Exp $ Au ["Knuth"]
     >     t1 = Exp $ Ti ["The Art of Computer Programming"]
     >     t2 = Exp $ Ti ["Concrete Mathematics"]
     >     ex = au /*/ (t1 /+/ t2)
     >  in ...
  -}

  {- $RequestOv
  
     Requests are URL parameters,
     either \"search_query\" or \"id_list\".
     This module provides functions
     to build and parse these parameters,
     to create the full request string
     and to navigate through a multi-page request
     with a maximum number of items per page.
    
     For details of the Arxiv request format,
     please refer to the Arxiv documentation.
  -}

  {- $ResponseOv
     
     Response processing expects [Tag String] as input (see TagSoup).
     The result produced by your http library
     (such as http-conduit) must be converted to [Tag String]
     before the result is passed to the response functions 
     defined here (see also the example below).

     The response functions extract information from the tag soup,
     either in 'String', 'Int' or TagSoup format.

     For details of the Arxiv Feed format, please refer 
     to the Arxiv documentation.
  -}

  {- $CompleteExample

     > module Main
     > where
     >
     >   import qualified Network.Api.Arxiv as Ax
     >   import           Network.Api.Arxiv (Expression(..), 
     >                                Field(..), (/*/), (/+/))
     >   import           Network.Socket (withSocketsDo)
     >   import           Network.HTTP.Simple as HT
     >   import           Network.HTTP.Conduit (parseRequest)
     >   import           Network.HTTP.Types.Status
     >   import           Data.List (intercalate)
     >   import qualified Data.ByteString as B hiding (unpack) 
     >   import qualified Data.ByteString.Char8 as B  (unpack) 
     >   import           Data.Conduit ((.|))
     >   import qualified Data.Conduit as C
     >   import qualified Data.Conduit.List as CL
     >   import           Data.Function ((&))
     >   import           Text.HTML.TagSoup
     >   import           Control.Monad.Trans (liftIO)
     >   import           Control.Monad.Trans.Resource (MonadResource)
     >   import           Control.Applicative ((<$>))
     > 
     >   main :: IO ()
     >   main = withSocketsDo (execQuery makeQuery)
     >     
     >   makeQuery :: Ax.Query
     >   makeQuery = 
     >     let au = Exp $ Au ["Aaronson"]
     >         t1 = Exp $ Ti ["quantum"]
     >         t2 = Exp $ Ti ["complexity"]
     >         x  = au /*/ (t1 /+/ t2)
     >      in Ax.Query {
     >           Ax.qExp   = Just x,
     >           Ax.qIds   = [],
     >           Ax.qStart = 0,
     >           Ax.qItems = 25}
     > 
     >   type Soup = Tag String
     > 
     >   execQuery :: Ax.Query -> IO ()
     >   execQuery q = C.runConduitRes (searchAxv q .| outSnk)
     > 
     >   ----------------------------------------------------------------------
     >   -- Execute query and start a source
     >   ----------------------------------------------------------------------
     >   searchAxv :: MonadResource m => Ax.Query -> C.ConduitT () String m ()
     >   searchAxv q = 
     >     let s = Ax.mkQuery q
     >      in do rsp <- HT.httpBS =<< liftIO (parseRequest s)
     >            case getResponseStatus rsp of
     >              (Status 200 _) -> getSoup (getResponseBody rsp)
     >                                >>= results q
     >              st             -> error $ "Error:" ++ show st
     > 
     >   ----------------------------------------------------------------------
     >   -- Consume page by page
     >   ----------------------------------------------------------------------
     >   getSoup :: MonadResource m =>  
     >              B.ByteString -> C.ConduitT () String m [Soup]
     >   getSoup b = concat <$> (C.yield b .| toSoup .| CL.consume)
     > 
     >   ----------------------------------------------------------------------
     >   -- Receive a ByteString and yield Soup
     >   ----------------------------------------------------------------------
     >   toSoup :: MonadResource m => C.ConduitT B.ByteString [Soup] m ()
     >   toSoup = C.awaitForever (C.yield . parseTags . B.unpack)
     > 
     >   ----------------------------------------------------------------------
     >   -- Yield all entries and fetch next page
     >   ----------------------------------------------------------------------
     >   results :: MonadResource m =>
     >              Ax.Query -> [Soup] -> C.ConduitT () String m ()
     >   results q sp = 
     >      if Ax.exhausted sp 
     >        then C.yield ("EOT: " ++ show (Ax.totalResults sp) ++ " results")
     >        else Ax.forEachEntryM_ sp (C.yield . mkResult) 
     >             >> searchAxv (Ax.nextPage q)
     >   
     >   ----------------------------------------------------------------------
     >   -- Get data and format
     >   ----------------------------------------------------------------------
     >   mkResult :: [Soup] -> String
     >   mkResult sp = let aus = Ax.getAuthorNames sp
     >                     y   = Ax.getYear sp
     >                     tmp = Ax.getTitle sp & clean ['\n', '\r', '\t']
     >                     ti  = if null tmp then "No title" else tmp
     >                  in intercalate ", " aus ++ " (" ++ y ++ "): " ++ ti
     >     where clean _ [] = []
     >           clean d (c:cs) | c `elem` d =   clean d cs
     >                          | otherwise  = c:clean d cs
     > 
     >   ----------------------------------------------------------------------
     >   -- Sink results 
     >   ----------------------------------------------------------------------
     >   outSnk :: MonadResource m => C.ConduitT String C.Void m ()
     >   outSnk = C.awaitForever (liftIO . putStrLn)
  -}

  ------------------------------------------------------------------------ 
  -- | The Arxiv base URL \"arxiv.org\"
  ------------------------------------------------------------------------ 
  baseUrl :: String
  baseUrl =  "arxiv.org"

  ------------------------------------------------------------------------ 
  -- | The Arxiv API URL \"export.arxiv.org/api\"
  ------------------------------------------------------------------------ 
  apiUrl :: String
  apiUrl =  "https://export.arxiv.org/api/query?"

  ------------------------------------------------------------------------ 
  -- | The query string (\"search_query=\" or \"id_list=\")
  ------------------------------------------------------------------------ 
  apiQuery,apiIdList :: String
  apiQuery  = "search_query="
  apiIdList = "id_list=" 

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
  --   The rationale is that the API specification is not clear
  --   on how expressions are parsed.
  --   The above expression could be understood as
  --   \"a AND (b OR c)\" or as \"(a AND b) OR c\".
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

  type Identifier = String

  ------------------------------------------------------------------------
  -- | Query data type.
  --
  --   You usually want to create a query like:
  --
  --   > let e = (Exp $ Au ["Aaronson"]) /*/ (
  --   >           (Exp $ Ti ["quantum"]) /+/
  --   >           (Exp $ Ti ["complexity"]))
  --   >  in Query {
  --   >        qExp   = Just e,
  --   >        qIds   = ["0902.3175v2","1406.2858v1","0912.3825v1"],
  --   >        qStart = 0,
  --   >        qItems = 10}
  ------------------------------------------------------------------------
  data Query = Query {
                 -- | The query expression
                 qExp   :: Maybe Expression,
                 -- | Id List
                 qIds :: [Identifier],
                 -- | The first item we want to see
                 qStart :: Int,
                 -- | The number of items we want to see
                 qItems :: Int}
    deriving (Eq, Show)

  -------------------------------------------------------------------------
  -- | Prepares the query to fetch 
  --   the next page adding \"items per page\" to \"start index\".
  -------------------------------------------------------------------------
  nextPage :: Query -> Query
  nextPage  q = let s = qStart q
                    i = qItems q
                 in q{qStart = s + i} 

  -------------------------------------------------------------------------
  -- | Checks whether the query is exhausted or not, i.e.
  --   whether all pages have been fetched already.
  --   The first argument is the entire response (not just a part of it).
  -------------------------------------------------------------------------
  exhausted :: [Tag String] -> Bool
  exhausted sp = startIndex sp >= totalResults sp

  -------------------------------------------------------------------------
  -- | Parses an expression from a string.
  --   Please refer to the Arxiv documentation for details
  --   on query format.
  --
  --   Just a minor remark here:
  --   The operators OR, AND and ANDNOT are case sensitive.
  --   \"andnot\" would be interpreted as part of a title, for instance:
  --   \"ti:functional+andnot+object+oriented\" is just one title;
  --   \"ti:functional+ANDNOT+object+oriented\" would cause an error,
  --   because a field identifier (ti, au, etc.) is expected after
  --   \"+ANDNOT+\".
  --
  --   The other way round: the field content itself 
  --   is not case sensitive, i.e. 
  --   \"ti:complexity\" or \"au:aaronson\" is the same as
  --   \"ti:Complexity\" and \"au:Aaronson\" respectively.
  --   This is a feature of the very arXiv API.
  --
  --   You may want to refer to the comments under
  --   'preprocess' and 'exp2str' for some more details
  --   on our interpretation of the Arxiv documentation.
  -------------------------------------------------------------------------
  parseQuery :: String -> Either String Expression
  parseQuery s = case parse expression "" $ preprocess s of
                   Left  e -> Left $ show e
                   Right e -> Right e

  -------------------------------------------------------------------------
  -- | Converts a string containing comma-separated identifiers 
  --   into a list of 'Identifier's.
  --   As stated already: No whitespace! 
  -------------------------------------------------------------------------
  parseIds :: String -> [Identifier]
  parseIds = S.endBy "," 

  -------------------------------------------------------------------------
  -- | This is an internal function used by 'parseQuery'.
  --   It may be occasionally useful for direct use:
  --   It replaces \" \", \"(\", \")\" and \"\"\" 
  --   by \"+\", \"%28\", \"%29\" and \"%22\"
  --   respectively.
  --
  --   Usually, these substitutions are performed
  --   when transforming a string to an URL, which should be done
  --   by your http library anyway (e.g. http-conduit).
  --   But this step is usually after parsing has been performed
  --   on the input string. (Considering a work flow like:
  --   parseQuery >>= mkQuery >>= parseUrl >>= execQuery.)
  --   The parser, however, accepts
  --   only the URL-encoded characters and, thus, some preprocessing
  --   may be necessary.
  --
  --   The other way round, this means 
  --   that you may use parentheses, spaces and quotation marks
  --   instead of the URL encodings.
  --   But be careful! Do not introduce two successive spaces -
  --   we do not check for whitespace!
  -------------------------------------------------------------------------
  preprocess :: String -> String
  preprocess = concatMap s2s . map tos
    where s2s "("  = "%28"
          s2s ")"  = "%29"
          s2s "\"" = "%22"
          s2s " "  = "+"
          s2s c    = c
          tos c    = [c]

  -------------------------------------------------------------------------
  -- | Generates the complete query string 
  --   including URL, 
  --             query expression,
  --             id list and 
  --             item control
  -------------------------------------------------------------------------
  mkQuery :: Query -> String
  mkQuery q   = apiUrl ++ qry ++ x ++ plus ++ apiIdList ++ is ++ itm
    where x   = case qExp q of
                  Nothing -> ""
                  Just e  -> exp2str e
          plus = case qExp q of
                   Nothing -> ""
                   Just _  -> "&"  
          qry = case qExp q of
                  Nothing -> ""
                  Just _  -> apiQuery
          is  = ids2str $ qIds q
          itm = items2str q

  -------------------------------------------------------------------------
  -- | Converts a list of 'Identifier' 
  --   to a string with comma-separated identifiers
  -------------------------------------------------------------------------
  ids2str :: [Identifier] -> String
  ids2str = foldr i2s "" 
    where i2s i [] = i 
          i2s i s  = i ++ "," ++ s

  -------------------------------------------------------------------------
  -- | Converts the query to a string containing only the item control
  -------------------------------------------------------------------------
  items2str :: Query -> String
  items2str q = itemControl (qStart q) (qItems q) 

  -------------------------------------------------------------------------
  -- | Generates the item control of a query string according
  --   to first item and results per page:
  --
  --   * 'Int': Start index for this page
  --   
  --   * 'Int': Number of results per page.
  -------------------------------------------------------------------------
  itemControl :: Int -> Int -> String
  itemControl s m = "&amp;start="       ++ show s ++
                    "&amp;max_results=" ++ show m

  -- ======================================================================
  -- result
  -- ======================================================================

  ------------------------------------------------------------------------
  -- | Total results of the query
  ------------------------------------------------------------------------
  totalResults :: [Tag String] -> Int
  totalResults = getN "opensearch:totalResults" 

  ------------------------------------------------------------------------
  -- | Start index of this page of results
  ------------------------------------------------------------------------
  startIndex :: [Tag String] -> Int
  startIndex = getN "opensearch:startIndex"

  ------------------------------------------------------------------------
  -- | Number of items per page
  ------------------------------------------------------------------------
  itemsPerPage :: [Tag String] -> Int
  itemsPerPage = getN "opensearch:itemsPerPage"

  ------------------------------------------------------------------------
  -- | Checks if the feed contains an error message, i.e.
  --
  --   * it has only one entry,
  --
  --   * the title of this entry is \"Error\" and
  --
  --   * its id field contains an error message,
  --         which is returned as 'Left'.
  --
  --   Apparently, this function is not necessary,
  --   since the Arxiv site returns error feeds
  --   with status code 400 ("bad request"), 
  --   which should be handled by your http library anyway.
  ------------------------------------------------------------------------
  checkForError :: [Tag String] -> Either String ()
  checkForError ts = case totalResults ts of
                       1 -> head $ forEachEntry ts $ \e ->
                              if getTitle e == "Error"
                                then Left $ getError e
                                else Right ()
                       _ -> Right ()
                    

  ------------------------------------------------------------------------
  -- | Get the first entry in the tag soup.
  --   The function returns a tuple of
  --
  --   * The entry (if any)
  --
  --   * The rest of the tag soup following the first entry.
  --
  --   With getEntry, we can build a loop through all entries
  --   in the result (which is actually implemented in 'forEachEntry').
  ------------------------------------------------------------------------
  getEntry :: [Tag String] -> ([Tag String],[Tag String])
  getEntry = element "entry"

  ------------------------------------------------------------------------
  -- | Loop through all entries in the result feed
  --   applying a function on each one.
  --   The results are returned as list.
  --   The function is similar to 'map' 
  --   with the arguments swapped (as in Foldable 'forM').
  --
  --   Arguments:
  --
  --   * ['Tag' 'String']: The TagSoup through which we are looping
  --
  --   * ['Tag' 'String'] -> r: The function we are applying per entry;
  --                            the TagSoup passed in to the function
  --                            represents the current entry.
  --
  --   Example:
  --
  --   > forEachEntry soup $ \e ->
  --   >   let y = case getYear e of
  --   >             "" -> "s.a."
  --   >             x  -> x
  --   >       a = case getAuthorNames e of
  --   >             [] -> "Anonymous"
  --   >             as -> head as ++ 
  --   >    in a ++ " (" ++ y ++ ")"
  --
  --   Would retrieve the name of the first author 
  --   and the year of publication (like "Aaronson (2013)") 
  --   from all entries.
  ------------------------------------------------------------------------
  forEachEntry :: [Tag String] -> ([Tag String] -> r) -> [r]
  forEachEntry = forEach "entry"

  ------------------------------------------------------------------------
  -- | Variant of 'forEachEntry' for monadic actions.
  ------------------------------------------------------------------------
  forEachEntryM :: Monad m =>
                   [Tag String] -> ([Tag String] -> m r) -> m [r]
  forEachEntryM = forEachM "entry"
                   
  ------------------------------------------------------------------------
  -- | Variant of 'forEachEntryM' for actions 
  --   that do not return a result.
  ------------------------------------------------------------------------
  forEachEntryM_ :: Monad m =>
                    [Tag String] -> ([Tag String] -> m ()) -> m ()
  forEachEntryM_ = forEachM_ "entry"

  ------------------------------------------------------------------------
  -- | Gets the full contents of the id field 
  --   (which contains an URL before the article identifier).
  --   The [Tag String] argument is expected to be a single entry.
  ------------------------------------------------------------------------
  getIdUrl :: [Tag String] -> String
  getIdUrl = getString "id"

  ------------------------------------------------------------------------
  -- | Gets the article identifier as it can be used
  --   in an \"id_list\" query, i.e. without the URL.
  --   The [Tag String] argument is expected to be a single entry.
  ------------------------------------------------------------------------
  getId :: [Tag String] -> String
  getId = pureId . getString "id"

  ------------------------------------------------------------------------
  -- Extract the pure article identifier from the id string
  ------------------------------------------------------------------------
  pureId :: String -> String
  pureId s = let i = toSlash 2 (reverse s) 
                 z = drop 6 i
              in case z of
                   ""    -> reverse i
                   '.':_ -> reverse $ toSlash 1 i
                   _     -> reverse i
    where toSlash :: Int -> String -> String
          toSlash i m = let x = takeWhile (/= '/') m
                         in if i == 1 then x 
                            else x ++ ('/' :
                                 toSlash (i-1) (drop (length x + 1) m))

  ------------------------------------------------------------------------
  -- Get the error message
  ------------------------------------------------------------------------
  getError :: [Tag String] -> String
  getError = pureError . getString "id"

  ------------------------------------------------------------------------
  -- Extract the pure error message from the id string
  ------------------------------------------------------------------------
  pureError :: String -> String
  pureError = drop 1 . dropWhile (/= '#')

  ------------------------------------------------------------------------
  -- | Gets the contents of the \"updated\" field in this entry, i.e.
  --   the date when the article was last updated.
  --   Be aware that there is another \"updated\" field
  --   right below the root node of the result.
  --   Make sure your are operating on an entry,
  --   not on the root node!
  ------------------------------------------------------------------------
  getUpdated :: [Tag String] -> String
  getUpdated = getString "updated"

  ------------------------------------------------------------------------
  -- | Gets the contents of the \"published\" field in this entry, i.e.
  --   the date when the article was last uploaded.
  ------------------------------------------------------------------------
  getPublished :: [Tag String] -> String
  getPublished = getString "published"

  ------------------------------------------------------------------------
  -- | Gets the year of the \"published\" field in this entry.
  ------------------------------------------------------------------------
  getYear :: [Tag String] -> String
  getYear sp = case getPublished sp of
                 "" -> "s.a."
                 s  -> takeWhile (/= '-') s

  ------------------------------------------------------------------------
  -- | Gets the title of this entry.
  ------------------------------------------------------------------------
  getTitle :: [Tag String] -> String
  getTitle = getString "title"

  ------------------------------------------------------------------------
  -- | Gets the summary of this entry.
  ------------------------------------------------------------------------
  getSummary :: [Tag String] -> String
  getSummary = getString "summary"

  ------------------------------------------------------------------------
  -- | Gets author''s comment (in \"arxiv:comment\") of this entry.
  ------------------------------------------------------------------------
  getComment :: [Tag String] -> String
  getComment = getString "arxiv:comment"

  ------------------------------------------------------------------------
  -- | Gets the journal information (in \"arxiv:journal_ref\")
  --   of this entry.
  ------------------------------------------------------------------------
  getJournal :: [Tag String] -> String
  getJournal = getString "arxiv:journal_ref"

  ------------------------------------------------------------------------
  -- | Gets the digital object identifier (in \"arxiv:doi\") 
  --   of this entry.
  ------------------------------------------------------------------------
  getDoi :: [Tag String] -> String
  getDoi = getString "arxiv:doi"

  ------------------------------------------------------------------------
  -- | The Link data type
  ------------------------------------------------------------------------
  data Link = Link {
                -- | The hyperlink
                lnkHref  :: String,
                -- | The link type (a MIME type)
                lnkType  :: String, 
                -- | The link title (e.g. \"pdf\" would be the link
                --                        where we find the article 
                --                        in pdf format)
                lnkTitle :: String,
                -- | the link relation (e.g. \"related\" would point
                --                           to the related information,
                --                           such as the pdf document)
                lnkRel   :: String}
    deriving (Show, Eq)

  ------------------------------------------------------------------------
  -- | Gets all links in the entry.
  ------------------------------------------------------------------------
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

  ------------------------------------------------------------------------
  -- | Gets only the pdf link of this entry (if any).
  ------------------------------------------------------------------------
  getPdfLink :: [Tag String] -> Maybe Link
  getPdfLink soup = case getLinks soup of
                      [] -> Nothing
                      ls -> find (\l -> lnkTitle l == "pdf") ls

  ------------------------------------------------------------------------
  -- | Gets the hyperlink to the pdf document of this entry (if any).
  ------------------------------------------------------------------------
  getPdf :: [Tag String] -> String
  getPdf soup = case getPdfLink soup of
                  Nothing -> ""
                  Just l  -> lnkHref l

  ------------------------------------------------------------------------
  -- | Category data type
  ------------------------------------------------------------------------
  data Category = Category {
                    -- | The category term (e.g. \"math-ph\")
                    catTerm   :: String,
                    -- | The category scheme
                    catScheme :: String}
    deriving (Show, Eq)

  ------------------------------------------------------------------------
  -- Make category from TagSoup
  ------------------------------------------------------------------------
  mkCat :: Tag String -> Category
  mkCat c = Category {
              catTerm   = fromMaybe "" $ getAt "term"   c,
              catScheme = fromMaybe "" $ getAt "scheme" c}

  ------------------------------------------------------------------------
  -- | Gets the categories of this entry.
  ------------------------------------------------------------------------
  getCategories :: [Tag String] -> [Category]
  getCategories soup = case element "category" soup of
                         ([],_)   -> []
                         (x:_,[]) -> [mkCat x]
                         (x:_,rs) -> mkCat x : getCategories rs

  ------------------------------------------------------------------------
  -- | Gets the primary category of this entry (if any).
  ------------------------------------------------------------------------
  getPrimaryCategory :: [Tag String] -> Maybe Category
  getPrimaryCategory soup = case element "arxiv:primary_category" soup of
                              ([],_)  -> Nothing
                              (x:_,_) -> Just (mkCat x)

  ------------------------------------------------------------------------
  -- | The Author data type
  ------------------------------------------------------------------------
  data Author = Author {
                  -- | Author name
                  auName :: String,
                  -- | Author Affiliation
                  auFil  :: String}
    deriving (Show, Eq)

  ------------------------------------------------------------------------
  -- | Gets the authors of this entry.
  ------------------------------------------------------------------------
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

  ------------------------------------------------------------------------
  -- | Gets the names of all authors of this entry.
  ------------------------------------------------------------------------
  getAuthorNames :: [Tag String] -> [String]
  getAuthorNames = go 
    where go s = case element "author" s of
                   ([],[]) -> []
                   (a,[])  -> [getString "name" a]
                   (a,r)   ->  getString "name" a : go r

  ------------------------------------------------------------------------
  -- Lookup attribute by name
  ------------------------------------------------------------------------
  getAt :: String -> Tag String -> Maybe String
  getAt a (TagOpen _ as) = lookup a as 
  getAt _ _              = Nothing

  ------------------------------------------------------------------------
  -- Find a 'TagText' and return the content
  ------------------------------------------------------------------------
  getString :: String -> [Tag String] -> String
  getString n soup = let (i,_) = element n soup 
                      in if null i then "" else findTxt i

  ------------------------------------------------------------------------
  -- Find a 'TagText' and return the contentas a 'Int'.
  -- If the tag is not found or the content is not a number,
  -- -1 is returned.
  ------------------------------------------------------------------------
  getN :: String -> [Tag String] -> Int
  getN key soup = case element key soup of
                    (k,_) -> case findTxt k of
                               "" -> -1
                               t  -> if all isDigit t then read t else -1

  ------------------------------------------------------------------------
  -- Get the content of a 'TagText'
  ------------------------------------------------------------------------
  findTxt :: [Tag String] -> String
  findTxt [] = ""
  findTxt (t:ts) = case t of
                     TagText x -> x
                     _         -> findTxt ts

  ------------------------------------------------------------------------
  -- Map a function to all occurences of an element in the soup
  ------------------------------------------------------------------------
  forEach :: String -> [Tag String] -> ([Tag String] -> r) -> [r]
  forEach nm soup f = case element nm soup of
                        ([],_) -> []
                        (e,rs) -> f e : forEach nm rs f

  ------------------------------------------------------------------------
  -- Variant of forEach for monadic actions
  ------------------------------------------------------------------------
  forEachM :: Monad m => 
              String  -> [Tag String] -> ([Tag String] -> m r) -> m [r]
  forEachM nm soup f = case element nm soup of
                         ([],_) -> return []
                         (e,rs) -> do r  <- f e 
                                      rr <- forEachM nm rs f
                                      return (r:rr)

  ------------------------------------------------------------------------
  -- Variant of forEachM for actions that do not return anything
  ------------------------------------------------------------------------
  forEachM_ :: Monad m => 
               String  -> [Tag String] -> ([Tag String] -> m ()) -> m ()
  forEachM_ nm soup f = case element nm soup of
                          ([],_) -> return ()
                          (e,rs) -> f e >> forEachM_ nm rs f 

  ------------------------------------------------------------------------
  -- Find occurrence of an element and
  -- return this element (open tag to close tag) and
  --        the rest of the soup behind the element.
  ------------------------------------------------------------------------
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

  ------------------------------------------------------------------------
  -- Expression Parser
  ------------------------------------------------------------------------
  type Parser a = Parsec String () a

  ------------------------------------------------------------------------
  -- Expression is something in parentheses or something
  --            that starts with a field
  ------------------------------------------------------------------------
  expression :: Parser Expression
  expression = try parentheses <|> fieldOperator

  ------------------------------------------------------------------------
  -- A field potentially followed by an operator
  ------------------------------------------------------------------------
  fieldOperator :: Parser Expression
  fieldOperator = do
    f <- field
    c <- try (char '+') <|> return ' '
    if c == ' ' then return   f
                else opAndArg f
 
  ------------------------------------------------------------------------
  -- Find an operator and an expression
  ------------------------------------------------------------------------
  opAndArg :: Expression -> Parser Expression
  opAndArg f = do
    o <- op
    void $ char '+'
    e <- expression
    return (o f e)

  ------------------------------------------------------------------------
  -- A field consists of a fieldId and a list of terms
  ------------------------------------------------------------------------
  field :: Parser Expression
  field = do 
    i  <- fieldId 
    ts <- terms
    return (Exp $ i ts)
     
  ------------------------------------------------------------------------
  -- The field ids
  ------------------------------------------------------------------------
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

  ------------------------------------------------------------------------
  -- A term may be quoted,
  -- otherwise we build terms as list of strings
  -- separated by '+'
  ------------------------------------------------------------------------
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

  ------------------------------------------------------------------------
  -- Checks if an operator follows without consuming input
  ------------------------------------------------------------------------
  isOp :: Parser Bool
  isOp =     try (void (lookAhead (string "+ANDNOT+")) >> return True)
         <|> try (void (lookAhead (string "+AND+"))    >> return True)
         <|> try (void (lookAhead (string "+OR+"))     >> return True)
         <|> return False

  ------------------------------------------------------------------------
  -- A quoted term
  ------------------------------------------------------------------------
  quoted :: Parser String
  quoted = do
    void $ string "%22"
    intercalate "+" <$> go
    where go = do
            t <- term
            s <- try (string "%22") <|> return ""
            if not (null s) then return [t] 
                            else do c <- anyChar 
                                    let t' = if c == '+' then t 
                                                         else t ++ [c]
                                    (t':) <$> go
    
  ------------------------------------------------------------------------
  -- A single term 
  ------------------------------------------------------------------------
  term :: Parser String
  term = do 
    c <- try (lookAhead anyChar) <|> onEof '%'
    if c `elem` "%+"
      then return ""
      else do x <- char c
              (x:) <$> term
  
  ------------------------------------------------------------------------
  -- Signal EOF by returning the specified char
  ------------------------------------------------------------------------
  onEof :: Char -> Parser Char
  onEof c = eof >> return c

  ------------------------------------------------------------------------
  -- An expression in parentheses,
  -- which may be followed by another expression
  ------------------------------------------------------------------------
  parentheses :: Parser Expression
  parentheses = do
    void $ string "%28"
    e <- expression
    void $ string "%29"
    c <- try (char '+') <|> try (lookAhead (char '%')) <|> onEof '.'
    case c of
      '+' -> opAndArg e
      _   -> return   e

  ------------------------------------------------------------------------
  -- Parse operator (note that it is essential to 
  -- to process "ANDNOT" before "AND"!
  ------------------------------------------------------------------------
  op :: Parser (Expression -> Expression -> Expression)
  op =       try (void (string "ANDNOT") >> return AndNot) 
         <|> try (void (string "OR")     >> return Or) 
         <|>     (void (string "AND")    >> return And)

