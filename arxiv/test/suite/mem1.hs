{-# Language BangPatterns #-}
module Main
where

  import           Network (withSocketsDo)
  import           Network.HTTP.Conduit
  import           Network.HTTP.Types.Status
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit (($$+-)) 
  import qualified Data.Conduit as C
  import           Control.Monad.Trans (liftIO)
  import           Control.Monad (forever)

  main :: IO ()
  main = withSocketsDo $ {- forever $ -- good -}
    withManager $ \m -> forever $ do -- bad 
     u   <- liftIO (parseUrl "http://hackage.haskell.org")
     rsp <- http u m 
     case responseStatus rsp of
       (Status 200 _) -> responseBody rsp $$+- C.awaitForever (liftIO . putStrLn . B.unpack)
       st             -> error $ "Error:" ++ show st
