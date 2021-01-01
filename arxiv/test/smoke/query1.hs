---------------------------------------------------------------------------
-- | Expects a query as argument, executes it and presents the result
--   in some standard format on standard output
---------------------------------------------------------------------------
module Main
where

  import qualified Network.Api.Arxiv as Ax

  import           Network.Socket (withSocketsDo)
  import qualified Network.HTTP.Simple as HT
  import           Network.HTTP.Conduit (parseRequest)
  import           Network.HTTP.Types.Status
  import           Data.List (intercalate)
  import qualified Data.ByteString as B hiding (unpack) 
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit ((.|))
  import qualified Data.Conduit as C
  import qualified Data.Conduit.List as CL
  import           Text.HTML.TagSoup
  import           Control.Monad.IO.Class (MonadIO, liftIO)
  import           System.Environment

  e1 :: String
  e1 = "I need a query!"

  main :: IO ()
  main = withSocketsDo $ do
    os <- getArgs
    case os of
      []  -> error e1
      [q] -> execQuery $ makeQuery q
      _   -> print e1

  type Soup = Tag String

  makeQuery :: String -> Ax.Query
  makeQuery s = case Ax.parseQuery s of
                  Left  e -> error e
                  Right x -> Ax.Query {
                               Ax.qExp   = Just x,
                               Ax.qIds   = [],
                               Ax.qStart = 0,
                               Ax.qItems = 25}

  execQuery :: Ax.Query -> IO ()
  execQuery q = C.runConduitRes (searchAxv q .| outSnk)

  ------------------------------------------------------------------------
  -- Execute query and start a source
  ------------------------------------------------------------------------
  searchAxv :: MonadIO m => Ax.Query -> C.ConduitT () String m ()
  searchAxv q = 
    let s = Ax.mkQuery q
     in do rsp <- HT.httpBS =<< liftIO (parseRequest s)
           case HT.getResponseStatus rsp of
             (Status 200 _) -> getSoup (HT.getResponseBody rsp) >>= results q
             st             -> error $ "Error:" ++ show st

  ------------------------------------------------------------------------
  -- Consume page by page
  ------------------------------------------------------------------------
  getSoup :: MonadIO m =>  
             B.ByteString -> C.ConduitT () String m [Soup]
  getSoup b = concat <$> (C.yield b .| toSoup .| CL.consume)

  ------------------------------------------------------------------------
  -- Receive a ByteString and yield Soup
  ------------------------------------------------------------------------
  toSoup :: MonadIO m => C.ConduitT B.ByteString [Soup] m ()
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  ------------------------------------------------------------------------
  -- Yield results page by page
  ------------------------------------------------------------------------
  results :: MonadIO m => Ax.Query -> [Soup] -> C.ConduitT () String m ()
  results q sp = 
    let x = Ax.totalResults sp
        i = Ax.startIndex   sp
        e = Ax.itemsPerPage sp
     in if x /= 0 && x >= i
          then do 
            Ax.forEachEntryM_ sp (C.yield . mkResult) 
            searchAxv q{Ax.qStart = i + e} -- could be simple nextPage
          else C.yield ("EOT: " ++ show x ++ " results.")
  
  ------------------------------------------------------------------------
  -- Format result
  ------------------------------------------------------------------------
  mkResult :: [Soup] -> String
  mkResult sp = let aus = Ax.getAuthorNames sp
                    y   = Ax.getYear sp
                    tmp = Ax.getTitle sp
                    ti  = if null tmp then "No title" else tmp
                 in intercalate ", " aus ++ " (" ++ y ++ "): " ++ ti

  ------------------------------------------------------------------------
  -- Print it on stdout
  ------------------------------------------------------------------------
  outSnk :: MonadIO m => C.ConduitT String C.Void m ()
  outSnk = C.awaitForever (liftIO . putStrLn)

