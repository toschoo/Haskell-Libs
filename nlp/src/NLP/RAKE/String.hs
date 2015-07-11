{-# Language BangPatterns #-}
module Rake
where

  import           Data.List (intercalate,isPrefixOf,
                              foldl',sort,sortBy,nub)
  import           Data.Char (toLower,isDigit,isPunctuation)
  import           Data.Text (Text)
  import           Data.Map (Map)
  import qualified Data.Map as M
  import qualified Data.Text as T
  import qualified NLP.Tokenize.Text as NT

  import Debug.Trace (trace)

  loadStopWords :: FilePath -> IO (Map String ())
  loadStopWords f = do
    !c <- readFile f 
    let l = filter flt $ norm $ lines c
    return (foldl' (\m x -> M.insert x () m) M.empty l)
    where flt l = not("#" `isPrefixOf` l) && not (null l)
          norm = map (map toLower) . map ignoreWhitespace

  nosplit :: String
  nosplit = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+','-','/']

  nopunc :: String
  nopunc = "-"

  nolist :: [String]
  nolist = ["-"]

  splitphrase :: String
  splitphrase = ".,;:!?" -- to be improved

  ignoreWhitespace :: String -> String
  ignoreWhitespace = takeWhile (/= ' ') . dropWhile (== ' ')

  wSplitter :: String -> [String]
  wSplitter = go "" 
    where go t []     | null t           = []
                      | otherwise        = [reverse t]
          go t (c:cs) | c `elem` nosplit = go (c:t) cs
                      | otherwise        = if null t then go "" cs
                                                     else (reverse t) : go "" cs

  pSplitter :: String -> [String]
  pSplitter = go ""
    where go t []     | null t          = []
                      | otherwise       = [mkp t]
          go t (c:cs) | punctuation c   = (mkp t) : go "" cs
                      | otherwise       = go (c:t) cs
          mkp = dropWhile (== ' ') . reverse
          punctuation c = isPunctuation c && not (c `elem` nopunc)

  kfinder :: Map String () -> String -> [String]
  kfinder m = go [] . wSplitter . map toLower
    where go t []     | null t       = []
                      | otherwise    = [mkk t]
          go t (c:cs) | stopword m c = if null t then go [] cs
                                                 else (mkk t) : go [] cs
                      | otherwise    = go (c:t) cs
          mkk = intercalate " " . reverse
          
  stopword :: Map String () -> String -> Bool
  stopword m s = case M.lookup s m of
                   Nothing -> s `elem` nolist
                   Just _  -> True

  type WordFreq  = (String,Double,Double)
  type WordScore = (String,Double)

  candidates :: Map String () -> [String] -> [WordScore]
  candidates m ps = let ks = concatMap (kfinder m) ps
                        ws = wordScores ks
                     in sortByScore $ nub (kwScores ws ks) -- check!!!

  kwScores :: Map String (Double,Double) -> [String] -> [WordScore]
  kwScores m = map (kwScore m)

  kwScore :: Map String (Double,Double) -> String -> WordScore
  kwScore m s = let ws = splitWords 0 s
                 in (s,sum $ map findScore ws)
    where findScore w = case M.lookup w m of
                          Nothing    -> 0
                          Just (f,d) -> (d+f) / f
    
  wordScores :: [String] -> Map String (Double,Double)
  wordScores = foldl' score M.empty . foldl' wordScore [] 
    where score m (x,f,d) = M.insertWith add x (f,d) m
          add (f1,d1) (f2,d2) = (f1+1,d1+d2)

  wordScore :: [WordFreq] -> String -> [WordFreq]
  wordScore wf s = let ws = splitWords 0 s
                       f  = fromIntegral $ length ws
                       d  = f - 1
                    in foldl' (inswf d) wf ws

  inswf :: Double -> [WordFreq] -> String -> [WordFreq]
  inswf d [] s = [(s,1,d)]
  inswf d' ((w,f,d):ws) s | w == s    = (w,f+1,d'+d):ws
                          | otherwise = (w,f,d) : inswf d' ws s 
  
  -------------------------------------------------------------------------
  -- One might wonder why we split strings into words,
  -- instead of using data type [String] instead of String 
  -- in the first place.
  -- However, splitWords adds some additional criteria
  -- (such as not being numeric) that influence the scoring
  -- of keywords, but not the selection in keywords.
  -- We therefore stick to this somewhat suboptimal construction
  -------------------------------------------------------------------------
  splitWords :: Int -> String -> [String]
  splitWords m s = filter flt $ wSplitter s
    where flt w = length w > m &&
                  not (null w) &&
                  not (numeric w)

  sortKeysBy :: (Ord a, Ord b) => 
                ((a,b) -> (a,b) -> Ordering) -> [(a,b)] -> [(a,b)]
  sortKeysBy f = sortBy f 

  sortByScore :: [WordScore] -> [WordScore]
  sortByScore = sortKeysBy bySnd
    where bySnd (_,b1) (_,b2) = compare b2 b1

  sortByWord :: [WordScore] -> [WordScore]
  sortByWord = sortKeysBy byFst
    where byFst (a1,_) (a2,_) = compare a1 a2

  numeric :: String -> Bool
  numeric s | null s            = False
            | not (hasDigits s) = False
            | otherwise = let h = head s
                           in (isDigit h || h == '-') && pnumeric (tail s)
    where pnumeric = all (\x -> isDigit x || x == '.' || x == ',')
          hasDigits [] = False
          hasDigits (c:cs) | isDigit c = True
                           | otherwise = hasDigits cs

  tstText1 = "Compatibility of systems of linear constraints over the set of natural numbers. Criteria of compatibility of a system of linear Diophantine equations, strict inequations, and nonstrict inequations are considered. Upper bounds for components of a minimal set of solutions and algorithms of construction of minimal generating sets of solutions for all types of systems are given. These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions can be used in solving all the considered types of systems and systems of mixed types."

  tstText2 = "Compatibility of systems of linear constraints over the set of natural numbers. Criteria of compatibility of a system of linear Diophantine equations, strict inequations, and nonstrict inequations are considered. Upper bounds  - an important topic - for components of a minimal set of solutions and algorithms of construction of minimal generating sets of solutions for all types of systems are given. These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions can be used in solving all the considered types of systems and systems of mixed types."

  test1 :: IO ()
  test1 = do
    m <- loadStopWords "rake/rsc/SmartStoplist.txt"
    print (candidates m $ pSplitter tstText1)

  test2 :: IO ()
  test2 = do
    m <- loadStopWords "rake/rsc/SmartStoplist.txt"
    print (candidates m $ pSplitter tstText2)
 
  test3 :: IO ()
  test3 = let ps = pSplitter tstText1
              ws = map T.pack $ concatMap wSplitter ps
              ts = filter (\x -> x /= stp && x /= cm) $ NT.tokenize (T.pack tstText1)
           in do print ws
                 print ts
                 print (ws == ts)
    where stp = T.pack "."
          cm  = T.pack ","

  test4 :: IO ()
  test4 = do
    m <- loadStopWords "rake/rsc/SmartStoplist.txt"
    t <- readFile "../corpora/math/txt/bachLiars.txt"
    print (filter (\(_,s) -> s > 1.0) $ candidates m $ pSplitter t) 
  
