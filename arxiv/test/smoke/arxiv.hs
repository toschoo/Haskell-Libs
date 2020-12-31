module Main
where

  import qualified Network.Api.Arxiv as Ax
  import           Network.Api.Arxiv (Query(..))
  import           Network.Socket (withSocketsDo)
  import qualified Network.HTTP.Simple as HT
  import           Network.HTTP.Conduit (Request(..), Response(..), parseRequest)
  import           Network.HTTP.Types.Status
  import           Network.HTTP.Types.Header
  import           Data.List (intercalate, isPrefixOf)
  import qualified Data.ByteString as B hiding (pack,unpack) 
  import qualified Data.ByteString.Char8 as B  (pack,unpack) 
  import           Data.Conduit ((.|))
  import qualified Data.Conduit as C
  import           Data.Conduit.Binary (sinkFile) 
  import qualified Data.Conduit.List as CL
  import           Data.Function ((&))
  import           Text.HTML.TagSoup
  import           Control.Monad (unless)
  import           Control.Monad.IO.Class (MonadIO, liftIO)
  import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
  import           Control.Applicative ((<$>))
  import           Control.Exception
  import           System.Environment
  import           System.FilePath.Posix
  import           System.Directory

  type RIO = ResourceT IO
  type Soup = Tag String

  usage :: a
  usage = error ("I need a query in the format: "
                 ++ "<command> <subcommand> <query|id_list>\n"
                 ++ "where\n"
                 ++ "\tcommand:\n"
                 ++ "\t\tabstract              : prints the abstracts.\n"
                 ++ "\t\tcount                 : counts the number of results.\n"
                 ++ "\t\tformat <format string>: formats the result according to the format string;\n"
                 ++ "\t\t                        works similar to printf. The format string contains format specifiers\n"
                 ++ "\t\t                        starting with '%' followed by a field indicator ('au', 'ti', etc.)\n"
                 ++ "\t\t                        or '%' (in this case '%' is printed), e.g.\n"
                 ++ "\t\t                        \"%aus: %ti, %id (%idu), %y\"\n"
                 ++ "\t\t                        Note that escape sequences are not yet handled (todo).\n"
                 ++ "\t\tget                   : downloads the indicated enries;\n"
                 ++ "\t\t                        this is not allowed according to the arXiv terms of use!\n"
                 ++ "\t\tinfo                  : prints the query result.\n"
                 ++ "\tsubcommand:\n"
                 ++ "\t\tquery : executes a query in arXiv format.\n"
                 ++ "\t\tids   : searches for the articles with the comma-separated list of ids.\n\n"
                 ++ "Examples:\n"
                 ++ "\tarxiv format \"%aus: %ti, %id (%idu), %y\" \\\n"
                 ++ "\t      query  \"au:Aaronson+AND+(ti:quantum+OR+ti:complexity)\"\n"
                 ++ "\tarxiv count query \"au:Aaronson+AND+(ti:quantum+OR+ti:complexity)\"\n"
                 ++ "\tarxiv info  ids   \"2004.09674v7,1507.03546v5\"")

  main :: IO ()
  main = withSocketsDo $ do
    os <- getArgs
    case os of
      [] -> usage
      q  -> execCommand q 

  makeQuery :: String -> String -> Query
  makeQuery "" i = Query Nothing (Ax.parseIds i) 0 25
  makeQuery q  i = case Ax.parseQuery q of
                     Left  e -> error e
                     Right x -> Query {
                                  qExp   = Just x,
                                  qIds   = Ax.parseIds i,
                                  qStart = 0,
                                  qItems = 25}

  execCommand :: [String] -> IO ()
  execCommand []              = usage
  execCommand [_]             = usage
  execCommand ("count":xs)    = execCount xs
  
  execCommand ("get":xs)      = execGetSearch xs
  execCommand ("info":xs)     = execInfoSearch showResults  xs
  execCommand ("abstract":xs) = execInfoSearch showAbstract xs 
  execCommand ("format":xs)   = execInfoSearch (showFormat (head xs)) $ tail xs
  execCommand (x:_)           = error $ "Unknown command: " ++ x

  execInfoQuery :: C.ConduitT [Soup] String RIO () -> Query -> IO ()
  execInfoQuery c q = C.runConduitRes (searchEntries q .| c .| outSnk)

  execGetQuery :: Query -> FilePath -> IO ()
  execGetQuery q d = C.runConduitRes $
    searchEntries q .| pdfUrl .| savePdf d

  execCountQuery :: Query -> IO ()
  execCountQuery q = C.runConduitRes (countEntries q .| countC .| outSnk)

  execGetSearch :: [String] -> IO ()
  execGetSearch []  = usage
  execGetSearch [_] = usage
  execGetSearch ("query":xs) = 
    let q  = makeQuery (head xs) ""
        d' = makeDir   $ tail xs
        d  = if null d' then "." else d'
     in do t <- doesDirectoryExist d
           if not t then error $ d ++ " is not a directory or does not exist."
                    else execGetQuery q d
  execGetSearch ("ids":xs) = 
    let qs = getFilterQuery $ tail xs
        q  = makeQuery qs   $ head xs
        d' = if null qs then makeDir $ tail xs
                        else makeDir $ drop 2 xs
        d  = if null d' then "." else d'
     in do t <- doesDirectoryExist d
           if not t then error $ d ++ " is not a directory or does not exist."
                    else execGetQuery q d
  execGetSearch (x:_) = error $ "Unknown identifier " ++ x

  getFilterQuery :: [String] -> String
  getFilterQuery ("filter":[]) = error "Missing filter!"
  getFilterQuery ("filter":xs) = head xs
  getFilterQuery _             = ""

  execInfoSearch :: C.ConduitT [Soup] String RIO () -> [String] -> IO ()
  execInfoSearch _ []           = usage
  execInfoSearch _ [_]          = usage
  execInfoSearch c ("query":xs) = execInfoQuery c $ makeQuery (head xs) ""
  execInfoSearch c ("ids":xs)   = let qs = getFilterQuery $ tail xs
                                   in execInfoQuery c $ makeQuery qs (head xs)

  execInfoSearch _ (x:_)        = error $ "Unknown identifier " ++ x

  execCount :: [String] -> IO ()
  execCount []           = usage
  execCount [_]          = usage
  execCount ("query":xs) = execCountQuery $ makeQuery (head xs) ""
  execCount ("ids":xs)   = let qs = getFilterQuery $ tail xs
                            in execCountQuery $ makeQuery qs (head xs)
  execCount (x:_)        = error $ "Unknown identifier " ++ x

  makeDir :: [String] -> String
  makeDir []        = "."
  makeDir [_]       = usage
  makeDir ("to":xs) = head xs
  makeDir (_:_)     = "."

  searchEntries :: (MonadResource m, MonadIO m) =>
                   Ax.Query -> C.ConduitT () [Soup] m ()
  searchEntries q = searchAxv (resultSource q) q

  countEntries :: (MonadResource m, MonadIO m) => Ax.Query -> C.ConduitT () [Soup] m ()
  countEntries = searchAxv blobSource

  searchAxv :: (MonadResource m, MonadIO m) =>
               ([Soup] -> C.ConduitT () [Soup] m ()) -> Ax.Query -> C.ConduitT () [Soup] m () 
  searchAxv src q = do
     rsp <- HT.httpBS =<< liftIO (mkRequest $ Ax.mkQuery q)
     case HT.getResponseStatus rsp of
       (Status 200 _) -> getSoup (HT.getResponseBody rsp) >>= src
       st             -> error $ "Error:" ++ show st

  resultSource :: (MonadResource m, MonadIO m) => 
                  Ax.Query -> [Soup] -> C.ConduitT () [Soup] m ()
  resultSource q sp = 
    case Ax.checkForError sp of
      Left  r  -> error $ "Error: " ++ r
      Right () -> unless (Ax.exhausted sp) $ do
                    Ax.forEachEntryM_ sp C.yield 
                    Ax.nextPage q & searchEntries

  blobSource :: (MonadResource m, MonadIO m) => [Soup] -> C.ConduitT () [Soup] m ()
  blobSource sp = case Ax.checkForError sp of
                    Left r   -> error $ "Error: " ++ r
                    Right () -> C.yield sp

  countC :: (MonadResource m, MonadIO m) => C.ConduitT [Soup] String m ()
  countC = C.awaitForever (C.yield . showRes . Ax.totalResults)
    where showRes = show 

  ------------------------------------------------------------------------
  -- Add user agent to request
  ------------------------------------------------------------------------
  addAgent :: Request -> Request
  addAgent x = x{requestHeaders = [(hUserAgent, B.pack "arx")]}

  ------------------------------------------------------------------------
  -- Create Request
  ------------------------------------------------------------------------
  mkRequest :: String -> IO Request
  mkRequest u = addAgent <$> parseRequest u

  getSoup :: MonadIO m => 
             B.ByteString -> (C.ConduitT () [Soup] m [Soup])
  getSoup b = concat <$> (C.yield b .| toSoup .| CL.consume)

  toSoup :: MonadIO m => C.ConduitT B.ByteString [Soup] m ()
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  pdfUrl :: MonadIO m => 
            C.ConduitT [Soup] String m ()
  pdfUrl = C.awaitForever $ \e -> let p = Ax.getPdf e 
                                   in unless (null p) $ C.yield p

  savePdf :: FilePath -> C.ConduitT String C.Void RIO ()
  savePdf d = C.awaitForever $ \l -> C.handleC ignoreEx $ 
    let f = takeFileName l <.> "pdf"
     in do rq  <- liftIO (mkRequest l)
           rsp <- HT.httpBS rq 
           case responseStatus rsp of 
             (Status 200 _) -> C.yield (responseBody rsp) .| sinkFile (d </> f)
             st             -> liftIO (putStrLn $ "Status ('save'): " ++ 
                                                  show st)

  showResults :: (MonadResource m, MonadIO m) => C.ConduitT [Soup] String m ()
  showResults = C.awaitForever (C.yield . mkResult)

  showAbstract :: (MonadResource m, MonadIO m) => C.ConduitT [Soup] String m ()
  showAbstract = C.awaitForever (C.yield . mkAbstract)

  showFormat :: (MonadResource m, MonadIO m) => String -> C.ConduitT [Soup] String m ()
  showFormat f = C.awaitForever (C.yield . formatString f)

  outSnk :: (MonadResource m, MonadIO m) => C.ConduitT String C.Void m ()
  outSnk = C.awaitForever (liftIO . putStrLn)

  ------------------------------------------------------------------------
  -- Log error and continue
  ------------------------------------------------------------------------
  ignoreEx :: MonadIO m => SomeException -> C.ConduitM i o m ()
  ignoreEx = liftIO . print -- print to stderr!

  mkResult :: [Soup] -> String
  mkResult sp = let i   = Ax.getId sp
                    aus = Ax.getAuthorNames sp
                    y   = Ax.getYear sp
                    tmp = Ax.getTitle sp
                    ti  = if null tmp then "No title" else tmp
                 in i ++ " - " ++ intercalate ", " aus ++ 
                    " (" ++ y ++ "): " ++ ti ++ "\n"

  mkAbstract :: [Soup] -> String
  mkAbstract sp = let i   = Ax.getId sp
                      a   = mainAuthor sp
                      y   = Ax.getYear sp
                      tmp = take 25 $ Ax.getTitle sp
                      ti  = if null tmp then "No title" else tmp
                      ab  = Ax.getSummary sp
                   in i ++ " - " ++ a ++ 
                      " (" ++ y ++ "): " ++ ti ++ "\n" ++ ab ++ "\n"

  formatString :: String -> [Soup] -> String
  formatString s sp = go s
    where go [] = ""
          go ('%':xs) | null xs   = ""
                      | head xs  == '%' = '%' : go (tail xs)
                      | otherwise = let (h, xs') = subst xs in h ++ go xs'
          go (x:xs) = x : go xs
          subst [] = ([], [])
          subst xs | "aus"  <| xs = (intercalate ", " $ Ax.getAuthorNames sp, drop 3 xs)
                   | "au"   <| xs = (mainAuthor sp, drop 2 xs)
                   | "ti"   <| xs = (Ax.getTitle sp, drop 2 xs)
                   | "idu"  <| xs = (Ax.getIdUrl sp, drop 3 xs)
                   | "id"   <| xs = (Ax.getId sp, drop 2 xs)
                   | "upd"  <| xs = (Ax.getUpdated sp, drop 3 xs)
                   | "pub"  <| xs = (Ax.getPublished sp, drop 3 xs)
                   | "y"    <| xs = (Ax.getYear sp, drop 1 xs)
                   | "res"  <| xs = (show $ Ax.totalResults sp, drop 3 xs)
                   | "idx"  <| xs = (show $ Ax.startIndex   sp, drop 3 xs)
                   | "abs"  <| xs = (Ax.getSummary sp, drop 3 xs)
                   | "jr"   <| xs = (Ax.getJournal sp, drop 2 xs)
                   | "doi"  <| xs = (Ax.getDoi sp, drop 3 xs)
                   | otherwise    = ("", xs)

  mainAuthor :: [Soup] -> String
  mainAuthor sp = let aus = Ax.getAuthorNames sp
                   in case length aus of
                        0 -> "Anonymous"
                        1 -> head aus
                        _ -> head aus ++ " et. al."

  infix <|
  (<|) :: String -> String -> Bool
  (<|) = isPrefixOf
