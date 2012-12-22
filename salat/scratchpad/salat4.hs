module Main
where

  import qualified Data.ByteString as B
  import qualified Data.ByteString.UTF8 as U
  import           Data.Enumerator (($$))
  import qualified Data.Enumerator as E hiding (($$))
  import qualified Data.Enumerator.List as EL
  import qualified Data.Enumerator.Binary as EB
  import qualified Data.Text as T
  import           Data.Text (Text(..))
  import qualified Data.Text.Encoding as E
  import qualified Data.Attoparsec.Text as A
  import           Parser
  import           Text.LaTeX.Base.Syntax
  import           Control.Monad.Trans (liftIO)
  import           System.IO (withBinaryFile, IOMode(..))
  import           System.Environment

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [f] -> do
        withBinaryFile f ReadMode $ \h -> do 
          ei_ <- E.run (EB.enumHandle 32 h $$ parse)
          case ei_ of
            Left  e -> putStrLn $ show e
            Right _ -> putStrLn "Ready"
      _ -> error "Filename!"

  out :: E.Iteratee B.ByteString IO ()
  out = out' B.empty

  out' :: B.ByteString -> E.Iteratee B.ByteString IO ()
  out' r = do
    mbi <- EL.head
    case mbi of
      Nothing -> return ()
      Just i  -> do (t', r') <- liftIO $ convert i r
                    liftIO (putStrLn   $ T.unpack t') >> out
                    

  parse :: E.Iteratee B.ByteString IO ()
  parse = parse' B.empty Nothing T.empty
  
  parse' :: B.ByteString -> Maybe (A.Result LaTeX) -> Text -> E.Iteratee B.ByteString IO ()
  parse' r mbR xx = do
    let parser = case mbR of
                   Nothing -> A.parse latexBlockParser
                   Just rc -> A.feed  rc
    mbi <- EL.head
    case mbi of
      Nothing -> if B.null r && T.null xx then liftIO $ putStrLn "Fim" 
                   else if not (B.null r) then error "Pending characters"
                          else case parser xx of
                                 A.Fail _ _ e     -> error "Parser failed!"
                                                     -- eof! --
                                 rx@(A.Partial _) -> error ("End of input reached: '" ++ T.unpack xx ++ "'")
                                 A.Done str l     -> do liftIO $ putStrLn (show l)
                                                        parse' B.empty Nothing str
      Just i  -> do
        (t, r') <- liftIO $ convert i r
        -- liftIO $ putStrLn (T.unpack t)
        case parser (xx `T.append` t) of
          A.Fail _ _ e     -> error "Parser failed!"
          rx@(A.Partial _) -> parse' r' (Just rx) T.empty
          A.Done str l     -> do liftIO $ putStrLn (show l)
                                 parse' r' Nothing  str

  convert :: B.ByteString -> B.ByteString -> IO (Text, B.ByteString)
  convert i r = do
    let (i', r')  = getCutOffChars (r `B.append` i)
    -- liftIO $ putStrLn $ show r
    -- liftIO $ putStrLn $ show i
    -- liftIO $ putStrLn $ show i'
    -- liftIO $ putStrLn $ show r'
    let eiT       = E.decodeUtf8' i'
    case eiT of
      Left  e -> error (show e)
      Right t -> return (t, r')

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
	
