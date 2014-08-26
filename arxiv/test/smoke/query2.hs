module Main
where

  import qualified Network.Api.Arxiv as Ax
  import           Network.Api.Arxiv (Expression(..), 
                                      Field(..), (/*/), (/+/))
  import           Network (withSocketsDo)
  import           Network.HTTP.Conduit
  import           Network.HTTP.Types.Status
  import           Data.List (intercalate)
  import qualified Data.ByteString as B hiding (unpack) 
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit (($$+-), (=$), ($$))
  import qualified Data.Conduit as C
  import qualified Data.Conduit.List as CL
  import           Text.HTML.TagSoup
  import           Control.Monad.Trans (liftIO)
  import           Control.Monad.Trans.Resource (MonadResource)
  import           Control.Applicative ((<$>))

  main :: IO ()
  main = withSocketsDo (execQuery makeQuery)
    
  makeQuery :: Ax.Query
  makeQuery = 
    let au = Exp $ Au ["Aaronson"]
        t1 = Exp $ Ti ["quantum"]
        t2 = Exp $ Ti ["complexity"]
        x  = au /*/ (t1 /+/ t2)
     in Ax.Query {
          Ax.qExp   = Just x,
          Ax.qIds   = [],
          Ax.qStart = 0,
          Ax.qItems = 25}

  type Soup = Tag String

  execQuery :: Ax.Query -> IO ()
  execQuery q = withManager $ \m -> searchAxv m q $$ outSnk

  ------------------------------------------------------------------------
  -- Execute query and start a source
  ------------------------------------------------------------------------
  searchAxv :: MonadResource m =>
               Manager -> Ax.Query -> C.Source m String
  searchAxv m q = 
    let s = Ax.mkQuery q
     in do u   <- liftIO (parseUrl s)
           rsp <- http u m 
           case responseStatus rsp of
             (Status 200 _) -> getSoup rsp >>= results m q 
             st             -> error $ "Error:" ++ show st

  ------------------------------------------------------------------------
  -- Consume page by page
  ------------------------------------------------------------------------
  getSoup :: MonadResource m => 
             Response (C.ResumableSource m B.ByteString) -> m [Soup]
  getSoup rsp = concat <$> (responseBody rsp $$+- toSoup =$ CL.consume)

  ------------------------------------------------------------------------
  -- Receive a ByteString and yield Soup
  ------------------------------------------------------------------------
  toSoup :: MonadResource m => C.Conduit B.ByteString m [Soup] 
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  ------------------------------------------------------------------------
  -- Yield all entries and fetch next page
  ------------------------------------------------------------------------
  results :: MonadResource m => 
             Manager -> Ax.Query -> [Soup] -> C.Source m String
  results m q sp = 
     if Ax.exhausted sp 
       then C.yield ("EOT: " ++ show (Ax.totalResults sp) ++ " results")
       else Ax.forEachEntryM_ sp (C.yield . mkResult) 
            >> searchAxv m (Ax.nextPage q)
  
  ------------------------------------------------------------------------
  -- Get data and format
  ------------------------------------------------------------------------
  mkResult :: [Soup] -> String
  mkResult sp = let aus = Ax.getAuthorNames sp
                    y   = Ax.getYear sp
                    tmp = Ax.getTitle sp
                    ti  = if null tmp then "No title" else tmp
                 in intercalate ", " aus ++ " (" ++ y ++ "): " ++ ti

  ------------------------------------------------------------------------
  -- Sink results 
  ------------------------------------------------------------------------
  outSnk :: MonadResource m => C.Sink String m ()
  outSnk = C.awaitForever (liftIO . putStrLn)
