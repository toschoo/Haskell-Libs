module Main
where

  import qualified Network.Api.Arxiv as Ax

  import           Network (withSocketsDo)
  import           Network.HTTP.Conduit
  import           Network.HTTP.Types.Status
  import           Data.List (intercalate)
  import qualified Data.ByteString as B hiding (unpack) 
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit (($$+-), (=$), ($$))
  import qualified Data.Conduit as C
  import qualified Data.Conduit.List as CL
  import           Control.Monad (void)
  import           Control.Monad.IO.Class (liftIO)
  import           Control.Monad.Trans.Resource (MonadResource)
  import           Control.Applicative ((<$>))
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
                               Ax.qExp   = x,
                               Ax.qStart = 0,
                               Ax.qItems = 25}

  execQuery :: Ax.Query -> IO ()
  execQuery q = withManager $ \m -> searchAxv m q $$ outSnk

  searchAxv :: MonadResource m =>
               Manager -> Ax.Query -> C.Source m String
  searchAxv m q = 
    let s = Ax.mkQuery q
     in do u   <- liftIO (parseUrl s)
           rsp <- http u m -- catch
           case responseStatus rsp of
             (Status 200 _) -> (responseBody rsp $$+- CL.consume) 
                                >>= results 
             st             -> error $ "Error:" ++ show st

  results :: MonadResource m => 
             [B.ByteString] -> C.Source m String
  results [] = return ()
  results (x:xs) = C.yield (B.unpack x) >> results xs
  
  outSnk :: MonadResource m => C.Sink String m ()
  outSnk = C.awaitForever (liftIO . putStrLn)
