module Main
where

  import qualified Data.ByteString as B
  import           Control.Monad.Trans (liftIO)
  import           System.Environment
  import           Text.LaTeX.Base.Parser
  import           Text.LaTeX.Base.Syntax (LaTeX(..))
  import           Text.LaTeX.Base.Render (render)
  import           Data.Text(Text)
  import           Data.Conduit (($$), ($=), (=$=))
  import qualified Data.Conduit        as C
  import qualified Data.Conduit.Binary as CB
  import qualified Data.Conduit.Text   as CT
  import qualified Data.Conduit.Util   as CU
  import           Data.Conduit.Attoparsec (conduitParser, PositionRange)

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [i] -> copy i
      _   -> error "I need an input file name!"

  copy :: FilePath -> IO ()
  copy i = C.runResourceT $ CB.sourceFile i   $= 
                            CT.decode CT.utf8 $= 
                            parse             $= 
                            manipulate        $=
                            renderC           $=
                            CT.encode CT.utf8 $$ out

  parse :: C.MonadResource m => C.Conduit Text m LaTeX
  parse = conduitParser latexBlockParser =$= sndC

  sndC :: C.MonadResource m => C.Conduit (PositionRange, LaTeX) m LaTeX
  sndC = C.awaitForever $ \(_, i) -> C.yield i

  renderC :: C.MonadResource m => C.Conduit LaTeX m Text
  renderC = CU.conduitState True push close
    where push  True  i = return $ CU.StateProducing (not $ isMainDoc i) [render i]
          push  False _ = return $ CU.StateFinished Nothing []
          close _       = return []

  out :: C.MonadResource m => C.Sink B.ByteString m () 
  out = C.awaitForever $ \i -> liftIO $ print i

  manipulate :: C.MonadResource m => C.Conduit LaTeX m LaTeX
  manipulate = C.awaitForever $ \i -> C.yield (apply i)
    where apply i = case i of
                      TeXComm s args | s == "textit" -> TeXComm "textbf" args
                                     | otherwise     -> TeXComm s        args
                      x                              -> x
