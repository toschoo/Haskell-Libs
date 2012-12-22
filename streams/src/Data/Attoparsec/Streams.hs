module Data.Attoparsec.Streams (
            convert, -- UTF8, UTF16, UTF32
            txtParserToEnumeratee,
            -- parserToIteratee
            -- byteParserToEnumeratee, 
            -- txtParserToIteratee,
            ignoreStream, continueHere,
            absorb, merge, pass, passBy, emit, runIt)
where

  import qualified Data.ByteString as B
  import           Data.Enumerator (($$), (=$), ($=))
  import qualified Data.Enumerator as E hiding (($$), (=$), ($=))
  import qualified Data.Enumerator.List as EL
  import qualified Data.Text as T
  import           Data.Text (Text)
  import qualified Data.Text.Encoding as Enc
  import           Data.Monoid (Monoid, mempty, (<>))
  import           Data.Sequence (Seq, ViewR(..), (|>), (<|), (><))
  import qualified Data.Sequence as S
  import           Data.Foldable (toList)
  import qualified Data.Attoparsec.Text as A
  import           Control.Monad.Trans (liftIO, lift)

  ------------------------------------------------------------------------
  -- | Parser
  ------------------------------------------------------------------------
  txtParserToEnumeratee :: (Monad m) => 
                           A.Parser o -> E.Enumeratee Text o m a
  txtParserToEnumeratee p = parsit Nothing T.empty

          -- do the job --------------------------------------------------
    where parsit mbR xx (E.Continue k) = do
            let (parser, ready) = case mbR of
                                    Nothing -> (A.parse p , True )
                                    Just rc -> (A.feed  rc, False)
            mbi <- EL.head
            case mbi of
              Nothing -> 
                if T.null xx && ready
                    then return (E.Continue k)
                    else go k parser  xx
              Just i  -> go k parser (xx <> i) 

          parsit _ _ stp = return stp 

          -- Apply the parser ----------------------------------------------
          go stp parser i =
            case parser i of
              A.Fail _ _ e     -> error e -- Better: E.throwError e
              rx@(A.Partial _) -> continue stp rx
              A.Done r o       -> done stp o r

          -- we are done ---------------------------------------------------
          done s o r   = lift (E.runIteratee $ s $ E.Chunks[o]) >>= 
                             parsit Nothing r

          -- still more work in the pipe ------------------------------------
          continue s f = lift (E.runIteratee $ s $ E.Chunks[ ]) >>= 
                             parsit (Just f) mempty


  ------------------------------------------------------------------------
  -- | Convert
  ------------------------------------------------------------------------
  convert :: E.Enumeratee B.ByteString Text IO a
  convert = convertit B.empty

          -- Do the job --------------------------------------------------
    where convertit :: B.ByteString -> E.Enumeratee B.ByteString Text IO a
          convertit r (E.Continue k) = do
             mbi <- EL.head
             case mbi of
               Nothing -> return (E.Continue k) -- E.continue k 
               Just i  -> let (i', r') = getCutOffChars (r `B.append` i)
                              eiT      = Enc.decodeUtf8' i'
                           in case eiT of
                                Left  e -> E.throwError e
                                Right o -> do
                                  stp <- lift (E.runIteratee $ k $ E.Chunks [o])
                                  convertit r' stp  
          convertit _ stp = return stp 

          -- keep multiple chars together ------------------------------------
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

  ------------------------------------------------------------------------
  -- | Transformers
  ------------------------------------------------------------------------
  type Transformer a b = Seq a -> E.Enumeratee a a IO b

  ignoreStream :: Transformer a b
  ignoreStream _ stp = EL.consume >>= \_ -> return stp

  continueHere :: Transformer a b
  continueHere _ stp = return stp

  emit :: Transformer a b -> Transformer a b
  emit go s stp = runIt (toList s) stp >>= go S.empty

  pass :: Transformer a b -> a -> Transformer a b
  pass go l _ stp = runIt [l] stp >>= go S.empty

  passBy :: Transformer a b -> a -> Transformer a b
  passBy go l s stp = runIt [l] stp >>= go s

  absorb :: Transformer a b -> a -> Transformer a b
  absorb go l s stp = go (s |> l) stp

  merge :: (Monoid a) => Transformer a b -> a -> Transformer a b
  merge go l s stp = let s' = case S.viewr s of
                                S.EmptyR  -> S.singleton l
                                xs :> x   -> xs |> (x <> l)
                      in go s' stp 

  runIt :: [x] -> E.Enumeratee x x IO a
  runIt s (E.Continue k) = lift (E.runIteratee $ k $ E.Chunks s) 
  runIt _ stp            = return stp
