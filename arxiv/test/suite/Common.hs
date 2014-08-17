module Common
where

  import           Test.QuickCheck
  import           Control.Applicative ((<$>))
  import           System.IO (stdout, hFlush)

  ------------------------------------------------------------------------
  -- For debugging it's much nicer to work with digits
  ------------------------------------------------------------------------
  data Digit = Digit Int
    deriving (Read, Eq, Ord)

  instance Show Digit where
    show (Digit d) = show d

  instance Arbitrary Digit where
    arbitrary = Digit <$> elements [1..9]

  ------------------------------------------------------------------------
  -- Ease working with either
  ------------------------------------------------------------------------
  infixl 9 ~>
  (~>) :: IO Bool -> IO Bool -> IO Bool
  x ~> f = x >>= \t -> if t then f else return False

  runX :: String -> Int -> IO () -> IO ()
  runX s n f = putStr ("Running Test " ++ s ++ ":    ") 
               >> hFlush stdout >> go n
    where go 0 = countdown 0 0 >> putStrLn ""
          go i = countdown i (i+1) >> f >> go (i-1)

  countdown :: Int -> Int -> IO ()
  countdown n o = 
    let x  = show o ++ " "
        b  = map (\_ -> '\b') x
        ch = b ++ show n
     in do putStr " "
           putStr ch >> hFlush stdout


  -------------------------------------------------------------
  -- controlled quickcheck, arbitrary tests
  -------------------------------------------------------------
  deepCheck :: (Testable p) => p -> IO Result
  deepCheck = quickCheckWithResult stdArgs{maxSuccess=100}

  -------------------------------------------------------------
  -- controlled quickcheck, arbitrary tests
  -------------------------------------------------------------
  someCheck :: (Testable p) => Int -> p -> IO Result
  someCheck n = quickCheckWithResult stdArgs{maxSuccess=n}

  -------------------------------------------------------------
  -- do just one test
  -------------------------------------------------------------
  oneCheck :: (Testable p) => p -> IO Result
  oneCheck = quickCheckWithResult stdArgs{maxSuccess=1}

  -------------------------------------------------------------
  -- combinator, could be a monad...
  -------------------------------------------------------------
  applyTest :: IO Result -> IO Result -> IO Result
  applyTest r f = do
    r' <- r
    case r' of
      Success {} -> f
      x          -> return x

  infixr ?>
  (?>) :: IO Result -> IO Result -> IO Result
  (?>) = applyTest

  -------------------------------------------------------------
  -- Name tests
  -------------------------------------------------------------
  runTest :: String -> IO Result -> IO Result
  runTest s t = putStrLn ("Test: " ++ s) >> t

