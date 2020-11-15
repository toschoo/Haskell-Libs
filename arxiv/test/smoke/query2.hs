{-# LANGUAGE OverloadedStrings #-}
module Main
where

  import qualified Network.Api.Arxiv as Ax
  import           Network.Api.Arxiv (Expression(..), 
                                      Field(..), (/*/), (/+/))
  import           Network.Socket (withSocketsDo)
  import           Network.HTTP.Simple as HT
  import           Network.HTTP.Conduit (parseRequest)
  import           Network.HTTP.Types.Status
  import           Data.List (intercalate)
  import qualified Data.ByteString as B hiding (unpack) 
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit ((.|))
  import qualified Data.Conduit as C
  import qualified Data.Conduit.List as CL
  import           Data.Function ((&))
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
  execQuery q = C.runConduitRes (searchAxv q .| outSnk)

  ------------------------------------------------------------------------
  -- Execute query and start a source
  ------------------------------------------------------------------------
  searchAxv :: MonadResource m => Ax.Query -> C.ConduitT () String m ()
  searchAxv q = 
    let s = Ax.mkQuery q
     in do u   <- liftIO (parseRequest s)
           -- rsp <- HT.httpSource u return
           rsp <- HT.httpBS u
           case getResponseStatus rsp of
             (Status 200 _) -> getSoup (getResponseBody rsp) >>= results q
             st             -> error $ "Error:" ++ show st

  ------------------------------------------------------------------------
  -- Consume page by page
  ------------------------------------------------------------------------
  getSoup :: MonadResource m =>  
             B.ByteString -> C.ConduitT () String m [Soup]
  getSoup b = concat <$> (C.yield b .| toSoup .| CL.consume)

  ------------------------------------------------------------------------
  -- Receive a ByteString and yield Soup
  ------------------------------------------------------------------------
  toSoup :: MonadResource m => C.ConduitT B.ByteString [Soup] m ()
  toSoup = C.awaitForever (C.yield . parseTags . B.unpack)

  ------------------------------------------------------------------------
  -- Yield all entries and fetch next page
  ------------------------------------------------------------------------
  results :: MonadResource m => Ax.Query -> [Soup] -> C.ConduitT () String m ()
  results q sp = 
     if Ax.exhausted sp 
       then C.yield ("EOT: " ++ show (Ax.totalResults sp) ++ " results")
       else Ax.forEachEntryM_ sp (C.yield . mkResult) 
            >> searchAxv (Ax.nextPage q)
  
  ------------------------------------------------------------------------
  -- Get data and format
  ------------------------------------------------------------------------
  mkResult :: [Soup] -> String
  mkResult sp = let aus = Ax.getAuthorNames sp
                    y   = Ax.getYear sp
                    tmp = Ax.getTitle sp & cleanLn ['\n', '\r', '\t']
                    ti  = if null tmp then "No title" else tmp
                 in intercalate ", " aus ++ " (" ++ y ++ "): " ++ ti
    where cleanLn _ [] = []
          cleanLn d (c:cs) | c `elem` d =   cleanLn d cs
                           | otherwise  = c:cleanLn d cs

  ------------------------------------------------------------------------
  -- Sink results 
  ------------------------------------------------------------------------
  outSnk :: MonadResource m => C.ConduitT String C.Void m ()
  outSnk = C.awaitForever (liftIO . putStrLn)
