{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (($$))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Network.HTTP.Simple          as HT
import           Network.HTTP.Conduit         (parseUrl)
import           System.IO                    (stdout)

url = "https://export.arxiv.org/api/query?search_query=au:Aaronson+AND+%28ti:quantum+OR+ti:complexity%29&id_list=&amp;start=0&amp;max_results=25"

main :: IO ()
main = do
    u <- parseUrl url
    print u
    runResourceT
        $ HT.httpSource u getSrc
       $$ CB.sinkHandle stdout
  where
    getSrc res = do
        liftIO $ print (HT.getResponseStatus res, HT.getResponseHeaders res)
        getResponseBody res
