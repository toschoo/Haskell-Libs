module Main
where

  import qualified Data.ByteString as B
  import           Data.Enumerator (($$), (=$)) 
  import qualified Data.Enumerator as E hiding (($$), (=$)) 
  import qualified Data.Enumerator.List as EL
  import qualified Data.Enumerator.Binary as EB
  import qualified Data.Text.Encoding as Enc
  import           Text.LaTeX.Base.Parser (isMainDoc) -- should be in Syntax
  import           Text.LaTeX.Base.Streams
  import           Text.LaTeX.Base.Syntax (LaTeX(..))
  import           Text.LaTeX.Base.Render (render)
  import           Control.Monad.Trans (liftIO)
  import           Control.Monad (unless)
  import           System.IO (Handle, withBinaryFile, IOMode(..))
  import           System.Environment

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [i,o] ->
        withBinaryFile i ReadMode $ \ih -> 
          withBinaryFile o WriteMode $ \oh -> do 
            ei_ <- E.run (EB.enumHandle 32 ih $$ 
                            convert =$ parse =$ manipulate =$ renderout oh)
            case ei_ of
              Left  e -> print e
              Right _ -> putStrLn "Ready"
      _ -> error "I need an input file name *and* and output file name!"

  renderout :: Handle -> E.Iteratee LaTeX IO ()
  renderout h = do
    mbi <- EL.head
    case mbi of
      Nothing -> return ()
      Just i  -> do liftIO (B.hPut h $ Enc.encodeUtf8 $ render i) 
                    unless (isMainDoc i) $ renderout h -- we stop when 
                                                       -- main document 
                                                       -- has been processed

  manipulate :: E.Enumeratee LaTeX LaTeX IO ()
  manipulate stp = do
    mbi <- EL.head
    case mbi of
      Nothing -> return stp
      Just i  -> runIt [apply i] stp >>= manipulate
    where apply i = case i of
                      TeXComm s args | s == "textit" -> TeXComm "textbf" args
                                     | otherwise     -> TeXComm s        args
                      x                              -> x
