{-# Language BangPatterns #-}
module Main
where

  -- import           Network (withSocketsDo)
  -- import           Network.HTTP.Conduit
  -- import           Network.HTTP.Types.Status
  import qualified Data.ByteString as B hiding (unpack) 
  import qualified Data.ByteString.Char8 as B  (unpack) 
  import           Data.Conduit (($$+-), ($=), (=$), ($$))
  import qualified Data.Conduit as C
  import qualified Data.Conduit.List as CL
  import qualified Data.Conduit.Binary as BL
  import           Control.Monad.Trans (liftIO)
  import           Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
  import           Control.Monad (forever,unless)
  import           Control.Applicative ((<$>))

  main :: IO ()
  main = runResourceT (src "mem1.hs" $$ snk)

  type RIO = ResourceT IO

  src :: FilePath -> C.Source RIO B.ByteString
  src p = BL.sourceFile p >> src p

  snk :: C.Sink B.ByteString RIO ()
  snk = C.awaitForever (liftIO . putStrLn . B.unpack)
