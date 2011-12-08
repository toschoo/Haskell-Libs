module Main
where

  import System.Exit
  import Test.QuickCheck
  import Test.QuickCheck.Monadic
  import qualified Data.IOMap as T
  import Prelude hiding (catch)
  import qualified Control.Exception as Exc
  import Control.Applicative ((<$>))
  import Data.List (nub, sort, delete, intersect)

  ------------------------------------------------------------------------
  -- For debugging it's much nicer to work with digits
  ------------------------------------------------------------------------
  data Digit = Digit Int
    deriving (Read, Eq, Ord)

  instance Show Digit where
    show (Digit d) = show d

  instance Arbitrary Digit where
    arbitrary = Digit <$> elements [0..9]

  ------------------------------------------------------------------------
  -- Make sure, we always insert!
  ------------------------------------------------------------------------
  data NubList a = NubList [a]
  
  instance (Show a) => Show (NubList a) where
    show (NubList l) = show l

  instance (Arbitrary a, Eq a) => Arbitrary (NubList a) where
    arbitrary = NubList <$> nub <$> arbitrary

  els :: Int -> [Int] -> Gen [Int]
  els _ [] = return [] 
  els n l  = nub <$> pickInd (max n ((length l) `div` 2)) l
    where pickInd m is = 
            if m == 0 then return []
              else do
                i   <- choose (0, length is)
                is' <- pickInd (m-1) is
                return (i:is)

  -- tests:
  -- when inserted it is in there
  -- when deleted  it is not in there anymore
  -- toList = sort nub list
  -- insList
  -- fromList
  -- pop
  -- height
  -- count
  -- with + withAll: 
  --  - with plain values (e.g. Int) as val
  --  - with IO (e.g. write/read files)
  -- concurrent access with insert + with + withAll
  -- 

  ------------------------------------------------------------------------------
  -- toList == sort list
  ------------------------------------------------------------------------------
  prp_Order :: NonEmptyList Int -> Property
  prp_Order (NonEmpty is) = collect (length $ nub is) $ monadicIO $ do
    t <- run T.newTree
    run $ mapM_ (\x -> T.insert t upd x ()) is
    let is' = sort $ nub is
    l <- run $ T.toKeyList t
    assert $ is' == l

  ------------------------------------------------------------------------------
  -- height <= 1 + log2 (length input-list)
  ------------------------------------------------------------------------------
  prp_heightLog2 :: NonEmptyList Int -> Property
  prp_heightLog2 (NonEmpty is) = collect (length $ nub is) $ monadicIO $ do
    t <- run T.newTree
    run $ mapM_ (\x -> T.insert t upd x ()) is
    d <- run $ hDiff t
    assert $ d <= 1 

  ------------------------------------------------------------------------------
  -- toList == sort list
  ------------------------------------------------------------------------------
  prp_OrderDel :: Int -> NonEmptyList Int -> Property
  prp_OrderDel m (NonEmpty is) = collect (length $ nub is) $ monadicIO $ do
    t <- run T.newTree
    run $ mapM_ (\x -> T.insert t upd x ()) is
    let is' = sort $ nub is
    ds <- pick (els m is')
    let dis = allDelete ds is'
    run $ mapM_ (T.delete t) ds
    l <- run $ T.toKeyList t
    assert $ dis == l

  ------------------------------------------------------------------------------
  -- height <= 1 + log2 (length input-list)
  ------------------------------------------------------------------------------
  prp_heightLog2Del :: Int -> NonEmptyList Int -> Property
  prp_heightLog2Del m (NonEmpty is) = collect (length $ nub is) $ monadicIO $ do
    t <- run T.newTree
    run $ mapM_ (\x -> T.insert t upd x ()) is
    let is' = sort $ nub is
    ds <- pick (els m is')
    let dis = allDelete ds is'
    run $ mapM_ (T.delete t) ds
    d <- run $ hDiff t
    assert $ d <= 1 

  allDelete :: [Int] -> [Int] -> [Int]
  allDelete _ [] = []
  allDelete [] l = l
  allDelete (d:ds)  l = allDelete ds (delete d l)

  upd :: a -> a -> IO a
  upd _ = return 

  log2 :: Double -> Int
  log2 = ceiling . logBase 2

  hDiff = T.fold2 (diff1 . abs) (-) 0
    where diff1 x = if x > 1 then x else 1

  -------------------------------------------------------------
  -- controlled quickcheck, arbitrary tests
  -------------------------------------------------------------
  deepCheck :: (Testable p) => p -> IO Result
  deepCheck = quickCheckWithResult stdArgs{maxSuccess=10000,
                                           maxDiscard=50000}

  -------------------------------------------------------------
  -- do just one test
  -------------------------------------------------------------
  oneCheck :: (Testable p) => p -> IO Result
  oneCheck = quickCheckWithResult stdArgs{maxSuccess=1,
                                          maxDiscard=1}

  -------------------------------------------------------------
  -- combinator, could be a monad...
  -------------------------------------------------------------
  applyTest :: IO Result -> IO Result -> IO Result
  applyTest r f = do
    r' <- r
    case r' of
      Success _ -> f
      x         -> return x

  infixr ?>
  (?>) :: IO Result -> IO Result -> IO Result
  (?>) = applyTest

  checkAll :: IO ()
  checkAll = do
    let good = "OK. All Tests passed."
    let bad  = "Bad. Some Tests failed."
    r <- deepCheck prp_Order ?>
         deepCheck prp_heightLog2 ?> 
         deepCheck prp_OrderDel ?>
         deepCheck prp_heightLog2Del 
    case r of
      Success _ -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
