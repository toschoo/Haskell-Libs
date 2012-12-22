module Main
where

  import qualified Data.ByteString as B
  import           Data.Enumerator (($$), (=$), ($=), (=$=))
  import qualified Data.Enumerator as E hiding (($$), (=$), ($=))
  import qualified Data.Enumerator.List as EL
  import qualified Data.Enumerator.Binary as EB
  import qualified Data.Text as T
  import           Data.Text (Text)
  import qualified Data.Text.Encoding as Enc
  import qualified Data.Attoparsec.Text as A
  import           Text.LaTeX.Base.Parser
  import           Text.LaTeX.Base.Syntax (LaTeX(..))
  import           Text.LaTeX.Base.Render (render)
  import           Control.Monad.Trans (liftIO, lift)
  import           System.IO (withBinaryFile, IOMode(..))
  import           System.Environment

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [f] ->
        withBinaryFile f ReadMode $ \h -> do 
          ei_ <- E.run (EB.enumHandle 32 h $$ convert =$ parse =$ {- manipulate =$ -} renderout)
          case ei_ of
            Left  e -> print e
            Right _ -> putStrLn "Ready"
      _ -> error "Filename!"

  out :: E.Iteratee LaTeX IO ()
  out = do
    mbi <- EL.head
    case mbi of
      Nothing -> return ()
      Just i  -> do liftIO (print i) 
                    if isMainDoc i then return ()
                                   else out

  renderout :: E.Iteratee LaTeX IO ()
  renderout = do
    mbi <- EL.head
    case mbi of
      Nothing -> return ()
      Just i  -> do liftIO (print $ render i) 
                    if isMainDoc i then return ()
                                   else renderout

  parse :: E.Enumeratee T.Text LaTeX IO ()
  parse = parse' Nothing T.empty
  
  parse' :: Maybe (A.Result LaTeX) -> Text -> E.Enumeratee Text LaTeX IO ()
  parse' mbR xx (E.Continue k) = do
    let (parser, ready) = case mbR of
                            Nothing -> (A.parse latexBlockParser, True )
                            Just rc -> (A.feed  rc              , False)
    mbi <- EL.head
    case mbi of
      Nothing -> 
        if T.null xx && ready
            then return (E.Continue k)
            else go k parser  xx
      Just i  -> go k parser (xx `T.append` i) 

    where go stp parser i =
            case parser i of
              A.Fail _ _ e     -> error e
              rx@(A.Partial _) -> continue stp rx
              A.Done r o       -> done stp o r

          done s o r   = lift (E.runIteratee $ s $ E.Chunks[o]) >>= 
                           parse' Nothing  r

          continue s f = lift (E.runIteratee $ s $ E.Chunks[ ]) >>= 
                           parse' (Just f) T.empty

  parse' _ _ stp = return stp 
  
  convert :: E.Enumeratee B.ByteString T.Text IO ()
  convert = convert' B.empty

  convert' :: B.ByteString -> E.Enumeratee B.ByteString T.Text IO ()
  convert' r (E.Continue k) = do
     mbi <- EL.head
     case mbi of
       Nothing -> return (E.Continue k) -- E.continue k 
       Just i  -> let (i', r') = getCutOffChars (r `B.append` i)
                      eiT      = Enc.decodeUtf8' i'
                   in case eiT of
                        Left  e -> E.throwError e
                        Right o -> do
                          stp <- lift (E.runIteratee $ k $ E.Chunks [o])
                          convert' r' stp  
  convert' _ stp = return stp 

  getCutOffChars :: B.ByteString -> (B.ByteString, B.ByteString)
  getCutOffChars x = 
    let s  = min 5 (B.length x - 1 ) 
        c  = B.takeWhile (< 192) $ B.reverse $ B.drop (B.length x - s) x
        l  = B.length c 
        l' = l + 1
        i = x `B.index` (B.length x - l')
     in if (l < 1 && i >= 192) ||
           (l < 2 && i >= 224) ||
           (l < 3 && i >= 240) ||
           (l < 4 && i >= 248) 
          then B.splitAt (B.length x - l') x
          else (x, B.empty)
	
