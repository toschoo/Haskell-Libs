---------------------------------------------------------------------------
-- | Expects a query as argument, executes it and presents the result as is
---------------------------------------------------------------------------
module Main
where

  import qualified Network.Api.Arxiv as Ax
  import           Network.Socket (withSocketsDo)
  import qualified Network.HTTP.Simple as HT
  import           Network.HTTP.Conduit (parseRequest)
  import           Network.HTTP.Types.Status
  import qualified Data.ByteString as B hiding (unpack) 
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit ((.|))
  import qualified Data.Conduit as C
  import           Data.Function ((&))
  import           Text.HTML.TagSoup
  import           Control.Monad (unless)
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

  searchAxv :: MonadIO m => Ax.Query -> C.ConduitT () String m ()
  searchAxv q = 
    let s = Ax.mkQuery q
     in do rsp <- HT.httpBS =<< liftIO (parseRequest s)
           case HT.getResponseStatus rsp of
             (Status 200 _) -> results q (HT.getResponseBody rsp)
             st             -> error $ "Error:" ++ show st

  results :: MonadIO m => Ax.Query -> B.ByteString -> C.ConduitT () String m ()
  results q b | B.null b  = return ()
              | otherwise = let s  = B.unpack b
                                sp = s & parseTags -- exhausted...
                             in unless (Ax.exhausted sp) $ 
                                C.yield s >> searchAxv (Ax.nextPage q)
  
  outSnk :: MonadIO m => C.ConduitT String C.Void m ()
  outSnk = C.awaitForever (liftIO . putStrLn)

