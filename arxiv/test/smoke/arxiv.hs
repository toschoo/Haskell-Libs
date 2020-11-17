module Main
where

  import qualified Network.Api.Arxiv as Ax
  import           Network.Api.Arxiv (Query(..), Field (..), 
                                      Expression(..))

  import           Network.Socket (withSocketsDo)
  import           Network.HTTP.Conduit
  import           Network.HTTP.Types.Status
  import           Network.HTTP.Types.Header
  import           Data.List (intercalate, isPrefixOf)
  import qualified Data.ByteString as B hiding (pack,unpack) 
  import qualified Data.ByteString.Char8 as B  (pack,unpack) 
  import           Data.Conduit (($$+-), (=$), ($$))
  import qualified Data.Conduit as C
  import           Data.Conduit.Binary (sinkFile) 
  import qualified Data.Conduit.List as CL
  import           Text.HTML.TagSoup
  import           Control.Monad (unless, when)
  import           Control.Monad.IO.Class (liftIO)
  import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
  import           Control.Applicative ((<$>))
  import           Control.Exception
  import           System.Environment
  import           System.FilePath.Posix
  import           System.Directory

  import           Debug.Trace (trace)

  usage :: a
  usage = error "I need a query!"

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

  type Soup = Tag String

  execInfoQuery :: C.Conduit [Soup] RIO String -> Query -> IO ()
  execInfoQuery c q = withManager $ \m -> searchEntries m q $$ c =$ outSnk

  execGetQuery :: Query -> FilePath -> IO ()
  execGetQuery q d = withManager $ \m -> 
    searchEntries m q $$ pdfUrl =$ savePdf m d

  execCountQuery :: Query -> IO ()
  execCountQuery q = withManager $ \m -> countEntries m q $$ countC =$ outSnk

  execCommand :: [String] -> IO ()
  execCommand []              = usage
  execCommand [_]             = usage
  execCommand ("count":xs)    = execCount xs
  execCommand ("get":xs)      = execGetSearch xs
  execCommand ("info":xs)     = execInfoSearch showResults  xs
  execCommand ("abstract":xs) = execInfoSearch showAbstract xs 
  execCommand ("format":xs)   = execInfoSearch (showFormat (head xs)) $ tail xs
  execCommand (x:_)           = error $ "Unknown command: " ++ x

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

  execInfoSearch :: C.Conduit [Soup] RIO String -> [String] -> IO ()
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
  makeDir (x:_)     = "."

  searchEntries :: MonadResource m =>
                   Manager -> Ax.Query -> C.Source m [Soup]
  searchEntries m q = searchAxv (resultSource m q) m q

  countEntries :: MonadResource m =>
                   Manager -> Ax.Query -> C.Source m [Soup]
  countEntries = searchAxv blobSource

  searchAxv :: MonadResource m =>
               ([Soup] -> C.Source m [Soup]) ->
               Manager -> Ax.Query -> C.Source m [Soup]
  searchAxv src m q = do
     rq  <- {- trace (Ax.mkQuery q) $ -} liftIO (mkRequest $ Ax.mkQuery q)
     rsp <- http rq m -- catch
     case responseStatus rsp of
       (Status 200 _) -> getSoup rsp >>= src -- resultSource m q 
       st             -> error $ "Error:" ++ show st

  resultSource :: MonadResource m => 
                  Manager -> Ax.Query -> [Soup] -> C.Source m [Soup]
  resultSource m q sp = 
    case Ax.checkForError sp of
      Left  r  -> error $ "Error: " ++ r
      Right () -> let x = Ax.totalResults sp
                      i = Ax.startIndex   sp
                      e = Ax.itemsPerPage sp
                   in unless (Ax.exhausted sp) $ do 
                        Ax.forEachEntryM_ sp C.yield
                        searchEntries m $ Ax.nextPage q

  blobSource :: MonadResource m => [Soup] -> C.Source m [Soup]
  blobSource sp = case Ax.checkForError sp of
                    Left r   -> error $ "Error: " ++ r
                    Right () -> C.yield sp

  countC :: MonadResource m => C.Conduit [Soup] m String
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
  mkRequest u = addAgent <$> parseUrl u

  getSoup :: MonadResource m => 
             Response (C.ResumableSource m B.ByteString) -> m [Soup]
  getSoup rsp = concat <$> (responseBody rsp $$+- toSoup =$ CL.consume)

  toSoup :: MonadResource m => C.Conduit B.ByteString m [Soup] 
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  pdfUrl :: MonadResource m => 
            C.Conduit [Soup] m String
  pdfUrl = C.awaitForever $ \e -> let p = Ax.getPdf e 
                                   in unless (null p) $ C.yield p

  savePdf :: Manager -> FilePath -> C.Sink String RIO ()
  savePdf m d = C.awaitForever $ \l -> C.handleC ignoreEx $ 
    let f = takeFileName l <.> "pdf"
     in do rq  <- liftIO (mkRequest l)
           rsp <- http rq m
           case responseStatus rsp of 
             (Status 200 _) -> responseBody rsp $$+- sinkFile (d </> f)
             st             -> liftIO (putStrLn $ "Status ('save'): " ++ 
                                                  show st)

  showResults :: C.Conduit [Soup] RIO String
  showResults = C.awaitForever (C.yield . mkResult)

  showAbstract :: C.Conduit [Soup] RIO String
  showAbstract = C.awaitForever (C.yield . mkAbstract)

  showFormat :: String -> C.Conduit [Soup] RIO String
  showFormat f = C.awaitForever (C.yield . formatString f)

  outSnk :: MonadResource m => C.Sink String m ()
  outSnk = C.awaitForever (liftIO . putStr)

  type RIO = ResourceT IO

  ------------------------------------------------------------------------
  -- Log error and continue
  ------------------------------------------------------------------------
  ignoreEx :: SomeException -> C.ConduitM i o RIO ()
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
          subst xs | "aus"  <| xs = (intercalate ", " $ Ax.getAuthorNames sp, 
                                     drop 3 xs)
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
