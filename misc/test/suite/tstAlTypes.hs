module Main
where

  import Test.QuickCheck
  import Test.QuickCheck.Monadic
  import Data.List ((\\), intercalate)
  import Data.List.Utils (replace)
  import System.Exit
  import Debug.Trace (trace)
  import Control.Applicative ((<$>))
  import Control.Monad (when)
  import Text.HTML.TagSoup

  import Algebra.Types
  import Common

  {-
  instance Arbitrary Expression
    where arbitrary = arbExp 1

  arbExp :: Int -> Gen Expression
  arbExp m = do
    x <- choose (0,m)
    if x == 0 then arbAndOr (m+1) else arbField

  arbAndOr :: Int -> Gen Expression
  arbAndOr m = do
    o  <- elements [And,Or,AndNot]
    e1 <- arbExp m
    e2 <- arbExp m
    return $ o e1 e2

  arbField :: Gen Expression
  arbField = do
     c  <- arbConstructor
     ts <- arbTerms
     return (Exp $ c ts)

  arbConstructor :: Gen ([String] -> Field)
  arbConstructor = do 
    x <- choose (0,9) :: Gen Int
    case x of
      0 -> return Au
      1 -> return Ti
      2 -> return Abs
      3 -> return Co
      4 -> return Jr
      5 -> return Cat
      6 -> return Rn
      7 -> return Id
      _ -> return All

  arbTerms :: Gen [String]
  arbTerms = do
    n  <- choose (1,10) 
    ts <- go n
    i  <- quotes
    if (i == 1) then return ts
                else do t <- nTerms i 
                        x <- choose (0,length ts - 1)
                        let (h,r) = splitAt x ts
                        return (h ++ [t] ++ r)
    where go :: Int -> Gen [String]
          go 0 = return []
          go i = do
              s <- arbString
              (s:) <$> go (i-1) 
          nTerms :: Int -> Gen String
          nTerms 1 = arbString
          nTerms i = do s <- arbString 
                        r <- nTerms (i-1)
                        return (s ++ "+" ++ r)

  arbString :: Gen String
  arbString = choose (3,15) >>= go
    where go :: Int -> Gen String
          go 0 = return ""
          go i = do
            c <- choose ('a','z')
            (c:) <$> go (i-1)

  quotes :: Gen Int
  quotes = do t <- choose (1,5) :: (Gen Int)
              if t > 1 then return 1 
                       else do x <- choose (2,9)
                               return x
  -}

  main :: IO ()
  main = putStrLn "Algebra Test Suite" >> checkAll

  prpSmoke1 :: Bool
  prpSmoke1 =
    let a = Var "a"
        b = Var "b"
        c = add a b
     in pretty (mul c c) == "aa+2ab+bb"

  prpSmoke2 :: Bool
  prpSmoke2 =
    let a = Var "a"
        b = Var "b"
        c = add a (Neg b)
     in trace (pretty $ mul c c) $ pretty (mul c c) == "aa+2ab+bb"

  {-
  visualTest :: Show b => ([a] -> b) -> String -> Property
  visualTest sut = withMonadicTags $ \ts -> do
    run $ forEachEntryM_ ts (print . sut)
    assert True

  withTags :: ([Soup] -> r) -> String -> r
  withTags f = f . parseTags 

  withMonadicTags :: ([Soup] -> PropertyM IO r) -> String -> Property
  withMonadicTags f = monadicIO . f . parseTags 
 
  -}


  checkAll :: IO ()
  checkAll = do
    let good = "OK. All Tests passed."
    let bad  = "Bad. Some Tests failed."
    r <- oneCheck  prpSmoke1 {- ?>
         oneCheck  prpSmoke2 -}
    case r of
      Success {}-> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure


