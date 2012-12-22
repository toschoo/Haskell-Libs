module Main
where

  import           Control.Monad.Trans (liftIO)
  import           Control.Monad (unless)
  import           Control.Exception (try, SomeException)
  import           System.Directory (doesFileExist)
  import           System.Environment (getArgs, getEnv)
  import           Text.LaTeX.Base.Parser
  import           Text.LaTeX.Base.ConduitParser
  import           Text.LaTeX.Base.Syntax
  import           Text.LaTeX.Base.Render (render)
  import           Data.Text(Text)
  import qualified Data.Text as T
  import           Data.Conduit (($$), ($=), (=$=))
  import qualified Data.Conduit        as C
  import qualified Data.Conduit.Binary as CB
  import qualified Data.Conduit.Text   as CT
  import qualified Data.Conduit.List   as CL
  import           Data.List.Split (endBy)
  import           Data.Char       (toUpper)
  import           Data.Monoid

  import           Rules

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [c,i,o] -> doesFileExist i >>= \x -> 
                   if x then do b <- initBase
                                process c b i o
                                e <- doesFileExist o
                                unless e $ error "No output generated"
                        else error $ "File " ++ i ++ " does not exist!"
      _       -> error "I need an operation, an input and an output file name!"

  ------------------------------------------------------------------------
  -- Get texalog base dir
  ------------------------------------------------------------------------
  initBase :: IO String
  initBase = do
    eiV <- try $ getEnv "TEXALOG_HOME"
    case eiV of
      Left  e -> print (e::SomeException) >> return "."
      Right v -> return v

  ------------------------------------------------------------------------
  -- Transform i to o using cs
  ------------------------------------------------------------------------
  process :: String -> String -> String -> String -> IO ()
  process cs p i o = 
    let (a,b,c,d) = parseOps p cs 
        texbuf    = length b > 0 
        txtbuf    = length d > 0 
        l         = foldl (.) id (a ++ b)
        t         = foldl (.) id (c ++ d)
     in C.runResourceT $ CB.sourceFile i   $= 
                         CT.decode CT.utf8 $= 
                         parse             $=
                         buffer texbuf     $= 
                         tex2tex l         $=
                         renderC           $=
                         buffer txtbuf     $= 
                         txt2txt t         $=
                         CT.encode CT.utf8 $$ CB.sinkFile o

  ------------------------------------------------------------------------
  -- Command defines a function and whether it needs buffering
  ------------------------------------------------------------------------
  data Command =  Txt (Bool, Text  -> Text) 
                | Tex (Bool, LaTeX -> LaTeX)

  ------------------------------------------------------------------------
  -- Command classes:
  -- - Tex unbuffered
  -- - Tex buffered
  -- - Text unbuffered
  -- - Text buffered
  ------------------------------------------------------------------------
  getPart :: Command -> Int
  getPart (Tex (True, _)) = 1
  getPart (Tex (False,_)) = 2
  getPart (Txt (True, _)) = 3
  getPart (Txt (False,_)) = 4

  ------------------------------------------------------------------------
  -- Get LaTeX transformer
  ------------------------------------------------------------------------
  toLaTeX :: Command -> LaTeX -> LaTeX
  toLaTeX (Tex (_,f))     = f
  toLaTeX (Txt (_,_))     = undefined

  ------------------------------------------------------------------------
  -- Get Text transformer
  ------------------------------------------------------------------------
  toText :: Command -> Text -> Text
  toText  (Txt (_,f))     = f
  toText  (Tex (_,_))     = undefined

  ------------------------------------------------------------------------
  -- Parse comma-separated list of operations
  -- and partition the resulting list into command classes
  ------------------------------------------------------------------------
  parseOps :: String -> String -> ([LaTeX -> LaTeX],[LaTeX -> LaTeX],
                                   [Text  -> Text ],[Text  -> Text ])
  parseOps p = part4 getPart toLaTeX toText . map (parseOp p) . endBy ","

  ------------------------------------------------------------------------
  -- Get command
  ------------------------------------------------------------------------
  parseOp :: String -> String -> Command
  parseOp p s = case map toUpper $ white s of
                  "VERSIFY" -> Txt (False, versify)
                  "INCLUDE" -> Tex (True , include p)
                  _         -> error $ "Unknown operation: " ++ s
    where white = takeWhile (/= ' ') . dropWhile (== ' ')

  ------------------------------------------------------------------------
  -- Partition a list into 4 lists with different data types
  -- (Could be further abstracted)
  ------------------------------------------------------------------------
  part4 :: (a -> Int) -> (a -> b) -> (a -> c) -> [a] -> ([b], [b], [c], [c])
  part4 p tob toc s = go s ([],[],[],[])
    where go []     r         = r
          go (x:xs) (a,b,c,d) | p x == 1  = go xs (tob x:a,b,c,d) 
                              | p x == 2  = go xs (a,tob x:b,c,d)
                              | p x == 3  = go xs (a,b,toc x:c,d)
                              | p x == 4  = go xs (a,b,c,toc x:d)
                              | otherwise = undefined

  ------------------------------------------------------------------------
  -- LaTeX Parser Conduit
  ------------------------------------------------------------------------
  parse :: C.MonadResource m => C.Conduit Text m LaTeX
  parse = conduitParser latexBlockParser

  ------------------------------------------------------------------------
  -- Render output 
  ------------------------------------------------------------------------
  renderC :: C.MonadResource m => C.Conduit LaTeX m Text
  renderC = C.awaitForever $ \i -> C.yield (render i)

  ------------------------------------------------------------------------
  -- To stdout - just for debugging
  ------------------------------------------------------------------------
  out :: C.MonadResource m => C.Sink Text m () 
  out = C.awaitForever $ \i -> liftIO $ putStrLn $ T.unpack i

  ------------------------------------------------------------------------
  -- LaTeX Transformer Conduit
  ------------------------------------------------------------------------
  tex2tex :: C.MonadResource m => (LaTeX -> LaTeX) -> C.Conduit LaTeX m LaTeX
  tex2tex = x2x 

  ------------------------------------------------------------------------
  -- Text Transformer Conduit
  ------------------------------------------------------------------------
  txt2txt :: C.MonadResource m => (Text -> Text) -> C.Conduit Text m Text
  txt2txt = x2x

  ------------------------------------------------------------------------
  -- Transformer Conduit
  ------------------------------------------------------------------------
  x2x :: C.MonadResource m => (a -> a) -> C.Conduit a m a
  x2x proc = C.awaitForever $ \i -> C.yield (proc i)

  ------------------------------------------------------------------------
  -- Buffering
  ------------------------------------------------------------------------
  buffer :: (C.MonadResource m, Monoid a) => Bool -> C.Conduit a m a
  buffer True  = CL.sequence CL.consume =$= collapse 
  buffer False = putThrough

  ------------------------------------------------------------------------
  -- Mconcat Conduit
  ------------------------------------------------------------------------
  collapse :: (C.MonadResource m, Monoid a) => C.Conduit [a] m a
  collapse = C.awaitForever $ \i -> C.yield (mconcat i)

  ------------------------------------------------------------------------
  -- Do nothing conduit
  ------------------------------------------------------------------------
  putThrough :: (C.MonadResource m, Monoid a) => C.Conduit a m a
  putThrough = C.awaitForever $ \i -> C.yield i
