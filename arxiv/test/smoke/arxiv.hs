---------------------------------------------------------------------------
-- | A swiss army knife command line program
--   to process and present queries to stdout.
--   The program accepts either a query in arXiv format
--   or a comma-separated list of identifiers and
--   provides commands to process results, e.g.
--   present them in a standard format,
--   present them in a user-defined format,
--   present only certain fields or count the results.
--   The program is much more a showroom dummy for
--   the ArXiv library than a production-ready program.
--   To get there, much is still left to be done...
---------------------------------------------------------------------------
module Main
where

  import qualified Network.Api.Arxiv                   as Ax
  import           Network.Api.Arxiv (Query(..))
  import           Network.Socket (withSocketsDo)
  import qualified Network.HTTP.Simple                 as HT
  import           Network.HTTP.Conduit (Request(..),
                                         Response(..),
                                         parseRequest)
  import           Network.HTTP.Types.Status
  import           Network.HTTP.Types.Header
  import           Data.List (intercalate, isPrefixOf)
  import           Data.Char (isDigit, digitToInt)
  import qualified Data.ByteString                     as B hiding (pack,unpack) 
  import qualified Data.ByteString.Char8               as B  (pack,unpack) 
  import           Data.Conduit ((.|))
  import qualified Data.Conduit                        as C
  import           Data.Conduit.Binary (sinkFile) 
  import qualified Data.Conduit.List                   as CL
  import           Data.Function ((&))
  import           Text.HTML.TagSoup
  import           Control.Monad (unless)
  import           Control.Monad.IO.Class (MonadIO, liftIO)
  import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
  import           Control.Exception
  import           System.Environment
  import           System.FilePath.Posix
  import           System.Directory

  usage :: a
  usage = error ("I need a query in the format: "
                 ++ "<command> <subcommand> <query|id_list>\n"
                 ++ "where\n"
                 ++ "\tcommand:\n"
                 ++ "\t\tinfo                  : prints the query result in a standard format.\n"
                 ++ "\t\t                        equivalent to format \"%id - %aus (%y): %ti\"\n"
                 ++ "\t\tabstract              : prints the abstracts.\n"
                 ++ "\t\t                        equivalent to format \"%id - %au (%y): %25ti\\n%abs\\n\"\n"
                 ++ "\t\tcount                 : counts the number of results.\n"
                 ++ "\t\tformat <format string>: formats the result according to the format string;\n"
                 ++ "\t\t                        works similar to printf. The format string contains format specifiers\n"
                 ++ "\t\t                        starting with '%' followed by a field indicator ('au', 'ti', etc.)\n"
                 ++ "\t\t                        or '%' (in this case '%' is printed), e.g.\n"
                 ++ "\t\t                        \"%aus: %ti, %id (%idu), %y\"\n"
                 ++ "\t\t                        would print the author names, colon, title, etc.\n"
                 ++ "\t\t                        Note that 'aus' is not an arxiv field indicator.\n"
                 ++ "\t\t                        'aus' is all author names, 'au' is the first author (main author).\n"
                 ++ "\t\t                        The number of characters taken from each field can be limited\n"
                 ++ "\t\t                        by placing a positive integer between '%' and the indicator, e.g.\n"
                 ++ "\t\t                        %25abs would limit the abstract to 25 characters.\n"
                 ++ "\t\t                        0 is ignored; i.e. %0abs is the same as %abs.\n"
                 ++ "\t\tget [to <directoy>]   : downloads the indicated enries; and stores them in <directory>\n"
                 ++ "\t\t                        or the current working directory if <directory> is not given.\n"
                 ++ "\t\t                        Note that this is not allowed according to the arXiv terms of use!\n"
                 ++ "\tsubcommand:\n"
                 ++ "\t\tquery : executes a query in arXiv format.\n"
                 ++ "\t\tids   : searches for the articles with the comma-separated list of ids.\n\n"
                 ++ "Examples:\n"
                 ++ "\tarxiv format \"%aus: %ti, %id (%idu), %y\" \\\n"
                 ++ "\t      query  \"au:Aaronson+AND+(ti:quantum+OR+ti:complexity)\"\n"
                 ++ "\tarxiv count query \"au:Aaronson+AND+(ti:quantum+OR+ti:complexity)\"\n"
                 ++ "\tarxiv info  ids   \"2004.09674v7,1507.03546v5\"")

  type RIO = ResourceT IO
  type Soup = Tag String

  -------------------------------------------------------------------------
  -- Frankfurt am
  -------------------------------------------------------------------------
  main :: IO ()
  main = withSocketsDo $ do
    os <- getArgs
    case os of
      [] -> usage
      q  -> execCommand q 

  -------------------------------------------------------------------------
  -- execute top-level commands
  -------------------------------------------------------------------------
  execCommand :: [String] -> IO ()
  execCommand []              = usage
  execCommand [_]             = usage
  execCommand ("count":xs)    = execCount xs
  
  execCommand ("get":xs)      = execGetSearch xs
  execCommand ("info":xs)     = execInfoSearch showResults  xs
  execCommand ("abstract":xs) = execInfoSearch showAbstract xs 
  execCommand ("format":xs)   = execInfoSearch (showFormat (head xs)) $ tail xs
  execCommand (x:_)           = error $ "Unknown command: " ++ x

  -------------------------------------------------------------------------
  -- Info search
  -------------------------------------------------------------------------
  execInfoSearch :: C.ConduitT [Soup] String RIO () -> [String] -> IO ()
  execInfoSearch _ []           = usage
  execInfoSearch _ [_]          = usage
  execInfoSearch c ("query":xs) = execInfoQuery c $ makeQuery (head xs) ""
  execInfoSearch c ("ids":xs)   = let qs = getFilterQuery (tail xs)
                                   in execInfoQuery c $ makeQuery qs (head xs)
  execInfoSearch _ (x:_)        = error $ "Unknown identifier " ++ x

  -------------------------------------------------------------------------
  -- Count
  -------------------------------------------------------------------------
  execCount :: [String] -> IO ()
  execCount []           = usage
  execCount [_]          = usage
  execCount ("query":xs) = execCountQuery $ makeQuery (head xs) ""
  execCount ("ids":xs)   = let qs = getFilterQuery (tail xs)
                            in execCountQuery $ makeQuery qs (head xs)
  execCount (x:_)        = error $ "Unknown identifier " ++ x

  -------------------------------------------------------------------------
  -- Get search
  -------------------------------------------------------------------------
  execGetSearch :: [String] -> IO ()
  execGetSearch []  = usage
  execGetSearch [_] = usage
  execGetSearch ("query":xs) = 
    let q  = makeQuery (head xs) ""
        d' = makeDir   (tail xs)
        d  = if null d' then "." else d'
     in do t <- doesDirectoryExist d
           if not t then error $ d ++ " is not a directory or does not exist."
                    else execGetQuery q d
  execGetSearch ("ids":xs) = 
    let qs = getFilterQuery (tail xs)
        q  = makeQuery qs   (head xs)
        d' = if null qs then makeDir $ tail xs
                        else makeDir $ drop 2 xs
        d  = if null d' then "." else d'
     in do t <- doesDirectoryExist d
           if not t then error $ d ++ " is not a directory or does not exist."
                    else execGetQuery q d
  execGetSearch (x:_) = error $ "Unknown identifier " ++ x

  -------------------------------------------------------------------------
  -- Helper to get a query from a query string and a list of ids
  -- with some decisions made by us
  -------------------------------------------------------------------------
  makeQuery :: String -> String -> Query
  makeQuery "" i = Query Nothing (Ax.parseIds i) 0 25
  makeQuery q  i = case Ax.parseQuery q of
                     Left  e -> error e
                     Right x -> Query {
                                  qExp   = Just x,
                                  qIds   = Ax.parseIds i,
                                  qStart = 0,
                                  qItems = 25}

  -------------------------------------------------------------------------
  -- Run an info query
  -------------------------------------------------------------------------
  execInfoQuery :: C.ConduitT [Soup] String RIO () -> Query -> IO ()
  execInfoQuery c q = C.runConduitRes (searchEntries q .| c .| outSnk)

  -------------------------------------------------------------------------
  -- Run a get query
  -------------------------------------------------------------------------
  execGetQuery :: Query -> FilePath -> IO ()
  execGetQuery q d = C.runConduitRes $
    searchEntries q .| pdfUrl .| savePdf d

  -------------------------------------------------------------------------
  -- Run a count query
  -------------------------------------------------------------------------
  execCountQuery :: Query -> IO ()
  execCountQuery q = C.runConduitRes (countEntries q .| countC .| outSnk)

  -------------------------------------------------------------------------
  -- Get the query string on which the filter applies
  -------------------------------------------------------------------------
  getFilterQuery :: [String] -> String
  getFilterQuery ["filter"]    = error "Missing filter!"
  getFilterQuery ("filter":xs) = head xs
  getFilterQuery _             = ""

  -------------------------------------------------------------------------
  -- Get the target directoy (if available)
  -------------------------------------------------------------------------
  makeDir :: [String] -> String
  makeDir []        = "."
  makeDir [_]       = usage
  makeDir ("to":xs) = head xs
  makeDir (_:_)     = "."

  -------------------------------------------------------------------------
  -- Perform an arxiv query and produce entries
  -- Note that we need to pass the query
  -- to the result source and to searchAxv (that's a bit awkward...)
  -------------------------------------------------------------------------
  searchEntries :: (MonadResource m, MonadIO m) =>
                   Ax.Query -> C.ConduitT () [Soup] m ()
  searchEntries q = searchAxv (resultSource q) q

  -------------------------------------------------------------------------
  -- Perform an arxiv query and produce pages
  -------------------------------------------------------------------------
  countEntries :: (MonadResource m, MonadIO m) =>
                  Ax.Query -> C.ConduitT () [Soup] m ()
  countEntries = searchAxv blobSource

  -------------------------------------------------------------------------
  -- Perform an arxiv query and yield the result through 'src'
  -------------------------------------------------------------------------
  searchAxv :: (MonadResource m, MonadIO m) =>
               ([Soup] -> C.ConduitT () [Soup] m ()) -> Ax.Query ->
               C.ConduitT () [Soup] m () 
  searchAxv src q = do
     rsp <- HT.httpBS =<< liftIO (mkRequest $ Ax.mkQuery q)
     case HT.getResponseStatus rsp of
       (Status 200 _) -> getSoup (HT.getResponseBody rsp) >>= src
       st             -> error $ "Error:" ++ show st

  -------------------------------------------------------------------------
  -- Yield entries on the current page and go to the next page
  -------------------------------------------------------------------------
  resultSource :: (MonadResource m, MonadIO m) => 
                  Ax.Query -> [Soup] -> C.ConduitT () [Soup] m ()
  resultSource q sp = 
    case Ax.checkForError sp of
      Left  r  -> error $ "Error: " ++ r
      Right () -> unless (Ax.exhausted sp) $ do
                    Ax.forEachEntryM_ sp C.yield 
                    Ax.nextPage q & searchEntries -- recurse!

  -------------------------------------------------------------------------
  -- Yield the entire page
  -------------------------------------------------------------------------
  blobSource :: (MonadResource m, MonadIO m) =>
                [Soup] -> C.ConduitT () [Soup] m ()
  blobSource sp = case Ax.checkForError sp of
                    Left r   -> error $ "Error: " ++ r
                    Right () -> C.yield sp -- we do not recurse!

  -------------------------------------------------------------------------
  -- Transform result stream to tag soup
  -------------------------------------------------------------------------
  getSoup :: MonadIO m => 
             B.ByteString -> C.ConduitT () [Soup] m [Soup]
  getSoup b = concat <$> (C.yield b .| toSoup .| CL.consume)

  -------------------------------------------------------------------------
  -- Helper to parse the tags
  -------------------------------------------------------------------------
  toSoup :: MonadIO m => C.ConduitT B.ByteString [Soup] m ()
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  ------------------------------------------------------------------------
  -- Create Request
  ------------------------------------------------------------------------
  mkRequest :: String -> IO Request
  mkRequest u = addAgent <$> parseRequest u

  ------------------------------------------------------------------------
  -- Add user agent to request
  ------------------------------------------------------------------------
  addAgent :: Request -> Request
  addAgent x = x{requestHeaders = [(hUserAgent, B.pack "arx")]}

  -------------------------------------------------------------------------
  -- Receive a tag soup and transform it into the standard result format
  -------------------------------------------------------------------------
  showResults :: (MonadResource m, MonadIO m) => C.ConduitT [Soup] String m ()
  showResults = C.awaitForever (C.yield . formatString "%id - %aus (%y): %ti")

  -------------------------------------------------------------------------
  -- Receive a tag soup and transform it into the abstract format
  -------------------------------------------------------------------------
  showAbstract :: (MonadResource m, MonadIO m) => C.ConduitT [Soup] String m ()
  showAbstract = C.awaitForever (C.yield . formatString 
                         "%id - %au (%y): %25ti\n%abs\n")

  -------------------------------------------------------------------------
  -- Receive a tag soup and transform it into the custom format
  -- using a format string
  -------------------------------------------------------------------------
  showFormat :: (MonadResource m, MonadIO m) =>
                String -> C.ConduitT [Soup] String m ()
  showFormat f = C.awaitForever (C.yield . formatString f)

  -------------------------------------------------------------------------
  -- Receive entire pages, extract the total result and
  -- transform into count format (which is just a number)
  -------------------------------------------------------------------------
  countC :: (MonadResource m, MonadIO m) => C.ConduitT [Soup] String m ()
  countC = C.awaitForever (C.yield . formatString "%res\n")

  -------------------------------------------------------------------------
  -- Yield PDF link (for get query)
  -------------------------------------------------------------------------
  pdfUrl :: MonadIO m => 
            C.ConduitT [Soup] String m ()
  pdfUrl = C.awaitForever $ \e -> let p = Ax.getPdf e 
                                   in unless (null p) $ C.yield p

  -------------------------------------------------------------------------
  -- Receive a string and write it to standard output
  -------------------------------------------------------------------------
  outSnk :: (MonadResource m, MonadIO m) => C.ConduitT String C.Void m ()
  outSnk = C.awaitForever $ \s -> unless (null s) $
                                    if last s == '\n'
                                       then liftIO $ putStr   s
                                       else liftIO $ putStrLn s

  -------------------------------------------------------------------------
  -- Request PDF and store it (for get query)
  -------------------------------------------------------------------------
  savePdf :: FilePath -> C.ConduitT String C.Void RIO ()
  savePdf d = C.awaitForever $ \l -> C.handleC ignoreEx $ 
    let f = takeFileName l <.> "pdf"
     in do rq  <- liftIO (mkRequest l)
           rsp <- HT.httpBS rq 
           case responseStatus rsp of 
             (Status 200 _) -> C.yield (responseBody rsp) .| sinkFile (d </> f)
             st             -> liftIO (putStrLn $ "Status ('save'): " ++ show st)

  -------------------------------------------------------------------------
  -- Use the format string to transform the soup into the custom format
  -- Note that aus/au is not standard arxiv!
  -------------------------------------------------------------------------
  formatString :: String -> [Soup] -> String
  formatString s sp = escape $ go s
    where go [] = ""
          go ('%':xs) | null xs   = ""
                      | head xs  == '%' = '%' : go (tail xs)
                      | otherwise = let (f, xs1) = limit 0 xs
                                        (h, xs2) = subst f xs1
                                     in h ++ go xs2
          go (x:xs) = x : go xs
          limit _ [] = (id, [])
          limit n (x:xs) | isDigit x = limit (10*n + digitToInt x) xs
                         | n > 0     = (take n, x:xs)
                         | otherwise = (id, x:xs)
          subst _ [] = ([], [])
          subst f xs | "aus"  <| xs = (f $ intercalate ", " $ Ax.getAuthorNames sp, drop 3 xs)
                     | "au"   <| xs = (f $ mainAuthor sp, drop 2 xs)
                     | "ti"   <| xs = (f $ Ax.getTitle sp, drop 2 xs)
                     | "idu"  <| xs = (f $ Ax.getIdUrl sp, drop 3 xs)
                     | "id"   <| xs = (f $ Ax.getId sp, drop 2 xs)
                     | "upd"  <| xs = (f $ Ax.getUpdated sp, drop 3 xs)
                     | "pub"  <| xs = (f $ Ax.getPublished sp, drop 3 xs)
                     | "y"    <| xs = (f $ Ax.getYear sp, drop 1 xs)
                     | "res"  <| xs = (f $ show $ Ax.totalResults sp, drop 3 xs)
                     | "idx"  <| xs = (f $ show $ Ax.startIndex   sp, drop 3 xs)
                     | "abs"  <| xs = (f $ Ax.getSummary sp, drop 3 xs)
                     | "jr"   <| xs = (f $ Ax.getJournal sp, drop 2 xs)
                     | "doi"  <| xs = (f $ Ax.getDoi sp, drop 3 xs)
                     | otherwise    = ("", xs)
          escape []        = []
          escape ['\\']    = []
          escape ('\\':xs) = eseq (head xs) : escape (tail xs)
          escape (x:xs)    = x:escape xs
          eseq 'b'  = '\b'
          eseq 'f'  = '\f'
          eseq 'n'  = '\n'
          eseq 'r'  = '\r'
          eseq 't'  = '\t'
          eseq '\\' = '\\'
          eseq _    = ' ' 

  -------------------------------------------------------------------------
  -- Get the main author; this is not an arxiv function!
  -------------------------------------------------------------------------
  mainAuthor :: [Soup] -> String
  mainAuthor sp = let aus = Ax.getAuthorNames sp
                   in case length aus of
                        0 -> "Anonymous"
                        1 -> head aus
                        _ -> head aus ++ " et. al."

  -------------------------------------------------------------------------
  -- Make it nice
  -------------------------------------------------------------------------
  infix <|
  (<|) :: String -> String -> Bool
  (<|) = isPrefixOf

  ------------------------------------------------------------------------
  -- Log error and continue
  ------------------------------------------------------------------------
  ignoreEx :: MonadIO m => SomeException -> C.ConduitM i o m ()
  ignoreEx = liftIO . print -- print to stderr!
