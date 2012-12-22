module Main 
where

  import Data.List ((\\))

  import Text.LaTeX.Base.Syntax
  import Text.LaTeX.Base.Parser
  import Text.LaTeX.Base.Render

  import Test.QuickCheck

  import qualified Data.Text as T

  import System.Exit (exitSuccess)
  import System.Environment (getArgs)

  import Control.Applicative ((<$>))

  main :: IO ()
  main = do
    os <- getArgs
    r  <- case os of
           []     -> deepCheck      100 prpParse
           [n]    -> deepCheck (read n) prpParse
           (_:_)  -> error "Too many arguments"
    case r of
      Success {} -> exitSuccess
      _          -> error "Bad: Some tests failed"

  -- put, parse and compare equal -------------------------------------------
  prpParse :: LaTeX -> Bool
  prpParse l = -- collect (render l) $ True
    case latexAtOnce $ render l of
      Left  _ -> False
      Right x -> x == l 

  -- Check -------------------------------------------------------------------
  deepCheck :: (Testable p) => Int -> p -> IO Result
  deepCheck n = quickCheckWithResult stdArgs{maxSuccess=n,
                                             maxDiscard=5*n}
    
  -------------------------------------------------------------------------
  -- Random Tests
  -------------------------------------------------------------------------
  instance Arbitrary LaTeX where
    arbitrary = do
      i <- choose (1::Int,20::Int)
      case i of
        1  -> TeXRaw . T.pack <$> arbstring 10
        2  -> do c  <- choose ('a','z')
                 n  <- choose (1,5)
                 as <- arguments n
                 return $ TeXComm [c] as
        3  -> do c <- elements mySpecials
                 return $ TeXCommS [c]
        4  -> do nm <- arbstring 5
                 n  <- choose (1,5)
                 as <- arguments n
                 x  <- arbinenv
                 return $ TeXEnv nm as x
        5  -> do m <- arbmathtype -- math except Dollar
                 x <- arbitrary
                 return $ TeXMathX m x []
        6  -> do x <- arbdollar -- Dollar
                 return $ TeXMathX Dollar x []
        7  -> do a <- choose (1::Int,5::Int)
                 let (d,u) | a == 5    = (Just 10.0, "pt")
                           | otherwise = (Nothing,   "")
                 b <- choose (1::Int,3::Int)
                 let str = b == 3
                 return $ TeXLineBreak d u str
        8  -> TeXRaw . T.pack <$> arbstring 10 -- operator 
                                               -- currently not used
        9  -> TeXBraces <$> arbitrary
        10 -> TeXComment . T.pack <$> arbstring 10
        11 -> do x <- arbinseq -- ambigue structure
                 y <- arbinseq
                 return (x <> y)
        _  -> TeXRaw . T.pack <$> arbstring 10

  instance Arbitrary TeXArg where
    arbitrary = do 
      x <- arbitrary
      i <- choose (1::Int, 2::Int)
      case i of
        1 -> return $ OptArg x
        _ -> return $ FixArg x

  multiple :: Arbitrary a => Gen a -> Int -> Gen [a]
  multiple produce = go []
    where go xs n | n == 0    = return xs
                  | otherwise = do x <- produce
                                   go (x:xs) (n-1)

  arbstring :: Int -> Gen String
  arbstring = multiple (choose ('a','z'))

  arguments :: Int -> Gen [TeXArg]
  arguments = multiple arbitrary

  arbmathtype :: Gen MathType
  arbmathtype = do
    i <- choose (1::Int,5::Int)
    return $ case i of
               1 -> MathEnv
               2 -> DispEnv
               3 -> EqEnv
               4 -> Parentheses
               _ -> Square

  arbdollar :: Gen LaTeX
  arbdollar = do
    x <- arbitrary
    if check4math x
      then arbdollar
      else return x

  check4math :: LaTeX -> Bool
  check4math l =
    case l of 
      TeXMathX{}      -> True
      TeXComm  _ as   -> checkArgs4math as
      TeXEnv   _ as b -> if not (checkArgs4math as)
                           then check4math b
                           else True
      TeXBraces     b -> check4math b
      TeXSeq x y      -> if not (check4math x)
                           then check4math y
                           else True
      _               -> False

  checkArgs4math :: [TeXArg] -> Bool
  checkArgs4math []     = False
  checkArgs4math (x:xs) = checkArg4math x || checkArgs4math xs

  checkArg4math :: TeXArg -> Bool
  checkArg4math a =
    case a of
      OptArg l -> check4math l
      FixArg l -> check4math l
      _        -> False

  arbinenv :: Gen LaTeX
  arbinenv = do
    x <- arbitrary
    case x of
      TeXBraces _ -> arbinenv
      _           -> return x

  arbinseq :: Gen LaTeX
  arbinseq = do
    x <- arbitrary
    case x of
      TeXSeq _ _  -> arbinseq
      TeXRaw _    -> arbinseq
      TeXBraces _ -> arbinseq
      _           -> return x

  mySpecials :: String
  mySpecials = specials \\ "[]()\\"
