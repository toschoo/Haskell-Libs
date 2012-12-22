module Main
where

  import qualified Data.ByteString as B
  import           Data.Enumerator (($$))
  import qualified Data.Enumerator as E hiding (($$))
  import qualified Data.Enumerator.List as EL
  import qualified Data.Enumerator.Binary as EB
  import qualified Data.Text as T
  import qualified Data.Text.IO as TI
  import qualified Data.Text.Encoding as E
  import           Control.Monad.Trans (liftIO)
  import           System.IO (withBinaryFile, IOMode(..))

  main :: IO ()
  main = do
    withBinaryFile "longjap.txt" ReadMode $ \h -> do 
      ei_ <- E.run (EB.enumHandle 32 h $$ convert)
      case ei_ of
        Left  e -> putStrLn $ show e
        Right _ -> putStrLn "Ready"

  convert :: E.Iteratee B.ByteString IO ()
  convert = convert' B.empty

  convert' :: B.ByteString -> E.Iteratee B.ByteString IO ()
  convert' r = do
    mbi <- EL.head
    case mbi of
      Nothing -> if B.null r then return ()
                             else error "Pending characters"
      Just i  -> do
        let (i', r')  = getCutOffChars (r `B.append` i)
        let eiT       = E.decodeUtf8' i'
        case eiT of
          Left  e -> liftIO $ putStrLn $ show e
          Right t -> liftIO (TI.putStrLn t) >> convert' r'

  getCutOffChars :: B.ByteString -> (B.ByteString, B.ByteString)
  getCutOffChars x = 
    let c  = B.takeWhile (< 192) $ B.reverse $ B.drop (B.length x - 5) x
        l  = B.length c 
        l' = l + 1
        i = x `B.index` (B.length x - l')
     in if (l < 1 && i >= 192) ||
           (l < 2 && i >= 224) ||
           (l < 3 && i >= 240) ||
           (l < 4 && i >= 248) 
          then B.splitAt (B.length x - l') x
          else (x, B.empty)
	
