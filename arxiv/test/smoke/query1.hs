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
  import           Text.HTML.TagSoup
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

  type Soup = Tag String

  execQuery :: Ax.Query -> IO ()
  execQuery q = withManager $ \m -> searchAxv m q $$ outSnk

  searchAxv :: MonadResource m =>
               Manager -> Ax.Query -> C.Source m String
  searchAxv m q = 
    let s = Ax.mkQuery q
     in do u   <- liftIO (parseUrl s)
           rsp <- http u m -- catch
           case responseStatus rsp of
             (Status 200 _) -> getSoup rsp >>= results m q 
             st             -> error $ "Error:" ++ show st

  getSoup :: MonadResource m => 
             Response (C.ResumableSource m B.ByteString) -> m [Soup]
  getSoup rsp = concat <$> (responseBody rsp $$+- toSoup =$ CL.consume)

  toSoup :: MonadResource m => C.Conduit B.ByteString m [Soup] 
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  results :: MonadResource m => 
             Manager -> Ax.Query -> [Soup] -> C.Source m String
  results m q sp = 
    let x = Ax.totalResults sp
        i = Ax.startIndex   sp
        e = Ax.itemsPerPage sp
     in if x /= 0 && x >= i
          then do 
            Ax.forEachEntryM_ sp (C.yield . mkResult) 
            searchAxv m q{Ax.qStart = i + e}
          else C.yield ("EOT: " ++ show x ++ " results.")
  
  mkResult :: [Soup] -> String
  mkResult sp = let aus = Ax.getAuthorNames sp
                    y   = Ax.getYear sp
                    tmp = Ax.getTitle sp
                    ti  = if null tmp then "No title" else tmp
                 in intercalate ", " aus ++ " (" ++ y ++ "): " ++ ti

  outSnk :: MonadResource m => C.Sink String m ()
  outSnk = C.awaitForever (liftIO . putStrLn)





