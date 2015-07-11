module NLP.RAKE.Text
where

  import           Data.List (foldl',sort,sortBy,nub)
  import           Data.Char (toLower,isDigit,isPunctuation)
  import           Data.Text (Text)
  import qualified Data.Text as T
  import qualified Data.Text.IO as TIO
  import           Data.Map (Map)
  import qualified Data.Map as M
  import qualified NLP.Tokenize.Text as NT

  import           NLP.RAKE.Stopwords

  import Debug.Trace (trace)

  type WordScore = (Text,Double)

  candidates :: StopwordsMap -> [Text] -> [WordScore]
  candidates m ps = let ks = concatMap (kfinder m) ps
                        ws = wordScores ks
                     in sortByScore $ nub (kwScores ws ks) 

  keywords :: Text -> [WordScore]
  keywords = candidates defaultStoplist . pSplitter

  rawKeywords :: [WordScore] -> [Text]
  rawKeywords = map fst

  kwFromLim :: Double -> [WordScore] -> [WordScore]
  kwFromLim d = filter (limit d)

  kwFromAvg :: [WordScore] -> [WordScore]
  kwFromAvg ws = kwFromLim d ws
    where wsum = sum $ map snd ws
          wlen = fromIntegral $ length ws
          d    = wsum / wlen

  kwFromMedian :: [WordScore] -> [WordScore]
  kwFromMedian ws = kwFromLim d ws
    where wlen = length ws
          d = let (l,m) | even wlen = (2,take 2 $ drop (wlen `div` 2 - 1) ws)
                        | otherwise = (1,take 1 $ drop ((wlen-1) `div` 2) ws)
               in (sum $ map snd m) / l
  
  limit :: Double -> WordScore -> Bool
  limit d (_,s) = d <= s

  nosplit :: [Char]
  nosplit = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+','-','/']

  nopunc :: [Char]
  nopunc = "-"

  space :: Text
  space = T.singleton ' '

  wSplitter :: Text -> [Text]
  wSplitter = go []
    where go t cs | T.null cs && null t      = []
                  | T.null cs                = [mkw t]
                  | T.head cs `elem` nosplit = go (T.head cs:t) (T.tail cs)
                  | otherwise                = 
                    if null t then go [] (T.tail cs)
                              else (mkw t) : go [] (T.tail cs)
          mkw = T.pack . reverse

  pSplitter :: Text -> [Text]
  pSplitter = go []
    where go t cs | T.null cs && null t     = []
                  | T.null cs               = [mkp t]
                  | punctuation (T.head cs) = (mkp t) : go [] (T.tail cs)
                  | otherwise               = go (T.head cs:t) (T.tail cs)
          mkp = T.dropWhile (== ' ') . T.pack . reverse
          punctuation c = isPunctuation c && not (c `elem` nopunc)

  kfinder :: StopwordsMap -> Text -> [Text]
  kfinder m = go [] . wSplitter . T.toLower
    where go [] [] = []
          go t  [] = [mkk t]
          go t (w:ws) | stopword m w = if null t then go [] ws
                                                 else (mkk t) : go [] ws
                      | otherwise    = go (w:t) ws
          mkk = T.intercalate space . reverse

  type WordFreq  = (Text,Double,Double)
  type ScoreMap  = Map Text (Double,Double)

  kwScores :: ScoreMap -> [Text] -> [WordScore]
  kwScores m = map (kwScore m)

  kwScore :: ScoreMap -> Text -> WordScore
  kwScore m s = let ws = splitWords 0 s
                 in (s,sum $ map findScore ws)
    where findScore w = case M.lookup w m of
                          Nothing    -> 0
                          Just (f,d) -> (d+f) / f
    
  wordScores :: [Text] -> ScoreMap 
  wordScores = foldl' score M.empty . foldl' wordScore [] 
    where score m (x,f,d) = M.insertWith add x (f,d) m
          add (f1,d1) (f2,d2) = (f1+1,d1+d2)

  wordScore :: [WordFreq] -> Text -> [WordFreq]
  wordScore wf s = let ws = splitWords 0 s
                       f  = fromIntegral $ length ws
                       d  = f - 1
                    in foldl' (inswf d) wf ws

  inswf :: Double -> [WordFreq] -> Text -> [WordFreq]
  inswf d [] s = [(s,1,d)]
  inswf d' ((w,f,d):ws) s | w == s    = (w,f+1,d'+d):ws
                          | otherwise = (w,f,d) : inswf d' ws s 
  
  -------------------------------------------------------------------------
  -- One might wonder why we split strings into words,
  -- instead of using data type [Text] instead of Text
  -- in the first place.
  -- However, splitWords adds some additional criteria
  -- (such as not being numeric) that influence the scoring
  -- of keywords, but not the selection in keywords.
  -- We therefore stick to this somewhat suboptimal construction
  -- of creating Text and then splitting it to [Text] again.
  -------------------------------------------------------------------------
  splitWords :: Int -> Text -> [Text]
  splitWords m s = filter flt $ wSplitter s
    where flt w = T.compareLength w m == GT &&
                  not (T.null w) &&
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

  numeric :: Text -> Bool
  numeric s | T.null s          = False
            | not (hasDigits s) = False
            | otherwise = let h = T.head s
                           in (isDigit h || h == '-') && pnumeric (T.tail s)
    where pnumeric cs | T.null cs = True
                      | otherwise = 
                        let h = T.head cs 
                         in (isDigit h || h == '.' || h == ',') && 
                            pnumeric (T.tail cs)
          hasDigits cs | T.null cs           = False
                       | isDigit (T.head cs) = True
                       | otherwise           = hasDigits (T.tail cs)

