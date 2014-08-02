{-# LANGUAGE NoMonomorphismRestriction #-}
module Main
where

  import           Control.Monad.Trans (liftIO)
  import qualified Control.Exception as E
  import           Control.Applicative ((<$>))
  import           System.Directory (doesFileExist, getDirectoryContents)
  import           System.FilePath.Posix ((</>), 
                                          takeExtension, takeExtension, 
                                          extSeparator)
  import           System.Environment (getArgs)
  import           System.Exit 
  import qualified System.IO as IO
  import           Text.LaTeX.Base.Parser
  import           Text.LaTeX.Base.Syntax
  import           Text.LaTeX.Base.Render (render)
  import           Data.Text.Encoding (decodeUtf8)
  import           Data.Text(Text)
  import qualified Data.Text as T
  import           Data.Char (toLower)
  import           Data.Conduit (($$), ($=), (=$=)) 
  import qualified Data.Conduit        as C
  import qualified Data.Conduit.Binary as CB
  import qualified Data.Conduit.Text   as CT
  import qualified Data.Conduit.Util   as CU
  import           Data.Conduit.Attoparsec
  import qualified Data.ByteString as BS

  -- import           ConduitParser3
  -- import           Text.LaTeX.Base.ConduitParser

  ind,oud :: FilePath
  ind = "test/tex/in"
  oud = "test/tex/out"

  main :: IO ()
  main = do 
      os <- getArgs
      case os of
        []  -> do ok <- testAll
                  if ok then putStrLn "Ok. All tests passed."
                        else putStrLn "Bad. Some tests failed." >>
                               exitFailure
        [f] -> do ok <- testFile f
                  if ok then putStrLn "Ok." 
                        else putStrLn "Bad. Test failed." >> exitFailure
        _   -> error "Don't know what to do with all these arguments!"

  ------------------------------------------------------------------------
  -- Make list of files
  ------------------------------------------------------------------------
  mklist :: IO [FilePath]
  mklist = filter isTeX <$> getDirectoryContents ind
    where isTeX x = map toLower (takeExtension x) == teX
          teX     = extSeparator : "tex"

  ------------------------------------------------------------------------
  -- Test all files in directory
  ------------------------------------------------------------------------
  testAll :: IO Bool
  testAll = and <$> (mklist >>= mapM testFile)

  ------------------------------------------------------------------------
  -- Test individual file
  ------------------------------------------------------------------------
  testFile :: FilePath -> IO Bool
  testFile f = {-# SCC "mean" #-} do
    x <- doesFileExist (ind </> f) 
    if x then do 
      putStrLn $ "Testing " ++ f
      E.catch (inout f >> comp f) 
              (\e -> do putStrLn $ "Error on " ++ ind </> f ++ ": "
                        print (e::E.SomeException)
                        return False)
         else do putStrLn $ ind </> f ++ " does not exist."
                 return False

  ------------------------------------------------------------------------
  -- Transform i to o using conduits
  ------------------------------------------------------------------------
  inout :: FilePath -> IO ()
  inout f = C.runResourceT $ CB.sourceFile (ind </> f) $= 
                             CT.decode CT.utf8         $= 
                             parse                     $=
                             renderC                   $= 
                             CT.encode CT.utf8         $$ 
                             CB.sinkFile (oud </> f)

  ------------------------------------------------------------------------
  -- Compare input and output
  ------------------------------------------------------------------------
  comp :: FilePath -> IO Bool
  comp f = IO.withBinaryFile (ind </> f) IO.ReadMode $ \h1 ->
             IO.withBinaryFile (oud </> f) IO.ReadMode $ \h2 -> do
               c1 <- decodeUtf8 <$> BS.hGetContents h1
               c2 <- decodeUtf8 <$> BS.hGetContents h2
               case latexAtOnce c1 of
                 Left  _  -> error "Can't parse" -- should not happen
                 Right r1 -> case latexAtOnce c2 of
                               Left  e  -> do putStrLn $ "Can't parse " ++
                                                         "the shit I have " ++
                                                         "just generated: " ++ e
                                              return False
                               Right r2 -> 
                                 if r1 == r2 
                                   then return True
                                   else case inoutCompare r1 r2 of
                                          Left  e -> do
                                            putStrLn $ "Error in " ++ 
                                                       f ++ ": "   ++ e
                                            return False
                                          Right _ -> return True

  inoutCompare :: LaTeX -> LaTeX -> Either String ()
  inoutCompare l1 l2 = case texCompare l1 l2 of
                         Left (x, y) -> leftText x y
                         _           -> Right ()
    where leftText x y = Left $ "Difference in " ++ ":\n" ++
                                show x ++  "\n"  ++
                                show y 

  ------------------------------------------------------------------------
  -- Compare two LaTeX trees element for element
  ------------------------------------------------------------------------
  texCompare :: LaTeX -> LaTeX -> Either (LaTeX, LaTeX) ()
  texCompare l1@(TeXRaw t1) l2@(TeXRaw t2) | t1 == t2  = Right ()
                                           | otherwise = Left (l1, l2)
  texCompare l1@(TeXComm name1 args1)
             l2@(TeXComm name2 args2) | name1 /= name2 = Left (l1, l2)
                                      | otherwise      = 
                                          argCompare l1 l2 args1 args2

  texCompare l1@(TeXCommS name1)
             l2@(TeXCommS name2) | name1 /= name2 = Left (l1, l2)
                                 | otherwise      = Right ()

  texCompare l1@(TeXEnv name1 args1 c1)
             l2@(TeXEnv name2 args2 c2) | name1 /= name2 = Left (l1, l2)
                                        | otherwise      =
                                            case argCompare l1 l2 args1 args2 of
                                              Right () -> texCompare c1 c2
                                              Left  x  -> Left x

  texCompare l1@(TeXMath e1 m1) 
             l2@(TeXMath e2 m2) | e1 /= e2  = Left (l1, l2)
                                | otherwise = texCompare m1 m2

  texCompare l1@(TeXLineBreak m1 b1)
             l2@(TeXLineBreak m2 b2) | m1 /= m2  = Left (l1, l2)
                                     | b1 /= b2  = Left (l1, l2)
                                     | otherwise = Right ()

  texCompare (TeXBraces b1) 
             (TeXBraces b2) = texCompare b1 b2

  texCompare l1@(TeXComment c1)
             l2@(TeXComment c2) | c1 /= c2  = Left (l1, l2)
                                | otherwise = Right ()

  texCompare (TeXSeq s11 s12) 
             (TeXSeq s21 s22) = case texCompare s11 s21 of
                                  Right () -> texCompare s12 s22 
                                  Left x   -> Left x

  texCompare TeXEmpty TeXEmpty = Right ()
  
  texCompare x y = Left (x,y)

  ------------------------------------------------------------------------
  -- Compare two LaTeX arguments
  ------------------------------------------------------------------------
  argCompare :: LaTeX   -> LaTeX    -> 
               [TeXArg] -> [TeXArg] -> Either (LaTeX, LaTeX) ()
  argCompare _  _  [] [] = Right ()
  argCompare l1 l2 [] _  = Left (l1, l2)
  argCompare l1 l2 _  [] = Left (l1, l2)
  argCompare l1 l2 (OptArg b1:xs1) 
                   (OptArg b2:xs2) = case texCompare b1 b2 of
                                       Left x -> Left x
                                       Right () -> argCompare l1 l2 xs1 xs2 
  argCompare l1 l2 (FixArg b1:xs1) 
                   (FixArg b2:xs2) = case texCompare b1 b2 of
                                       Left x -> Left x
                                       Right () -> argCompare l1 l2 xs1 xs2 

  argCompare _ _ _ _ = Right ()

  ------------------------------------------------------------------------
  -- LaTeX Parser Conduit
  ------------------------------------------------------------------------
  parse :: C.MonadResource m => C.Conduit Text m LaTeX
  parse = conduitParser latexBlockParser =$= dropPosition -- latexAtOnce -- =$= sndC -- latexAtOnce
    where dropPosition = C.awaitForever $ \(_, i) -> C.yield i

  -- sndC :: C.MonadResource m => C.Conduit (PositionRange, LaTeX) m LaTeX 
  -- sndC = C.awaitForever $ \(_, i) -> C.yield i

  ------------------------------------------------------------------------
  -- Render output (dropping everything following the main doc)
  ------------------------------------------------------------------------
  {-
  renderDocC :: C.MonadResource m => C.Conduit LaTeX m Text
  renderDocC = CU.conduitState True push close
    where push  True  i = return $ CU.StateProducing (not $ isMainDoc i) 
                                                     [render i]
          push  False _ = return $ CU.StateFinished Nothing []
          close _       = return []
  -}

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
