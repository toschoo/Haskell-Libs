-------------------------------------------------------------------------------
-- |
-- Module     : NLP.RAKE.Text.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
-- The RAKE Text interface 
-------------------------------------------------------------------------------
module NLP.RAKE.Text (
                      -- * Keywords
                      WordScore, 
                      candidates, keywords, rawKeywords,

                      -- * Utitlities
                      sortByScore, sortByWord,
                      pSplitter,
                      defaultNosplit,

                      -- * Stopwords
                      -- $Stopwords
                      StopwordsMap,
                      mkStopwords, mkStopwordsStr,
                      stopword,
                      defaultStoplist,
                      smartStoplist, foxStoplist,
                      loadStopWords)
                      
where

  import           Data.List (foldl',sortBy,nub)
  import           Data.Char (isDigit,isPunctuation)
  import           Data.Text (Text)
  import qualified Data.Text as T
  import           Data.Map (Map)
  import qualified Data.Map as M

  import           NLP.RAKE.Stopwords

  -------------------------------------------------------------------------
  -- import Debug.Trace (trace)
  -------------------------------------------------------------------------

  {- $Stopwords

     The very heart of the RAKE algorithm is the use of stop words,
     a concept defined by NLP pioneer Hans Peter Luhn.
     Stop words are frequent words in a language that are considered
     to be void of specific semantics. They, of course, have 
     an important role in the language, but they do not
     identify the topic a specific document is about, e.g.
     \"is\", \"the\", \"of\" and so on.
     Stop words depend on the specific context of the documents
     to be analysed; there are, however, frequently used lists
     with wide applicability.

     The library comes with two stop word lists built in:
     the 'smartStoplist' and the 'foxStoplist', both for English.
     The list used by default is 'smartStoplist'.

     The user is free to define her own stop word list,
     which can be loaded from a file using 'loadStopWords'.
     The file format is simple:
     
       * Lines starting with \'#\' are ignored (comments);

       * Each line contains one word. 
  -}

  -------------------------------------------------------------------------
  -- | The result is a keyword candidate,
  --   a keyword consisting of one or more words
  --   and a score associated with this keyword.
  -------------------------------------------------------------------------
  type WordScore = (Text,Double)

  -------------------------------------------------------------------------
  -- | This interface provides most flexibility.
  --   It expects a 'Map' of stop words, a /nosplit/ list
  --   used by the word splitter and a text split into phrases.
  --   Users may pass in their own stop word list 
  --   (e.g. by loading it from a file, see 'loadStopWords')
  --   or one of the predefined lists ('smartStopwords', 'foxStopwords').
  -------------------------------------------------------------------------
  candidates :: StopwordsMap -> String -> [Text] -> [WordScore]
  candidates m nsp ps = let ks = concatMap (kfinder m nsp) ps
                            ws = wordScores nsp ks 
                         in sortByScore $ nub (kwScores ws nsp ks) 

  -------------------------------------------------------------------------
  -- | The 'keywords' function is a convenience interface
  --   that takes a couple of decisions internally:
  --   it uses the 'defaultStoplist' and the English language 
  --   /nosplit/ list and it splits the text
  --   into phrases using the 'pSplitter'.
  --
  --   The function is equivalent to
  --  
  --   > candidates defaultStoplist defaultNosplit . pSplitter
  --
  -------------------------------------------------------------------------
  keywords :: Text -> [WordScore]
  keywords = candidates defaultStoplist defaultNosplit . pSplitter

  -------------------------------------------------------------------------
  -- | Given a 'WordScore' list 'rawKeywords' 
  --   yields a list of the keywords without the score
  --   (it, hence, maps 'fst' to the 'WordScore' list).
  -------------------------------------------------------------------------
  rawKeywords :: [WordScore] -> [Text]
  rawKeywords = map fst

  -------------------------------------------------------------------------
  -- | Sort the 'WordScore' list by scores (descending!)
  -------------------------------------------------------------------------
  sortByScore :: [WordScore] -> [WordScore]
  sortByScore = sortBy bySnd
    where bySnd (_,b1) (_,b2) = compare b2 b1

  -------------------------------------------------------------------------
  -- | Sort the 'WordScore' list by words (ascending!)
  -------------------------------------------------------------------------
  sortByWord :: [WordScore] -> [WordScore]
  sortByWord = sortBy byFst
    where byFst (a1,_) (a2,_) = compare a1 a2

  -------------------------------------------------------------------------
  -- | List containing 'Char' at which we do not split words.
  --   Note that this list is based on English. 
  -------------------------------------------------------------------------
  defaultNosplit :: String
  defaultNosplit = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-/"

  -------------------------------------------------------------------------
  -- List of Chars containing exceptions from 'isPunctuation'
  -------------------------------------------------------------------------
  nopunc :: String
  nopunc = "-"

  -------------------------------------------------------------------------
  -- Text ' '
  -------------------------------------------------------------------------
  space :: Text
  space = T.singleton ' '

  -------------------------------------------------------------------------
  -- Word Splitter
  -------------------------------------------------------------------------
  wSplitter :: String -> Text -> [Text]
  wSplitter nosplit = go []
    where go t cs | T.null cs && null t      = []
                  | T.null cs                = [mkw t]
                  | T.head cs `elem` nosplit = go (T.head cs:t) (T.tail cs)
                  | otherwise                = 
                    if null t then         go [] (T.tail cs)
                              else mkw t : go [] (T.tail cs)
          mkw = T.pack . reverse

  -------------------------------------------------------------------------
  -- | Default phrase splitter. It splits phrases at characters
  --   in the punctuation category 
  --   (those for which 'isPunctuation' is 'True') 
  --   with the exception of \'-\'.
  -------------------------------------------------------------------------
  pSplitter :: Text -> [Text]
  pSplitter = go []
    where go t cs | T.null cs && null t     = []
                  | T.null cs               = [mkp t]
                  | punctuation (T.head cs) = mkp t : go [] (T.tail cs)
                  | otherwise               = go (T.head cs:t) (T.tail cs)
          mkp = T.dropWhile (== ' ') . T.pack . reverse
          punctuation c = isPunctuation c && c `notElem` nopunc

  -------------------------------------------------------------------------
  -- Continues adding words to a keywords until a stopword is found
  -------------------------------------------------------------------------
  kfinder :: StopwordsMap -> String -> Text -> [Text]
  kfinder m nosplit = go [] . wSplitter nosplit . T.toLower
    where go [] [] = []
          go t  [] = [mkk t]
          go t (w:ws) | stopword m w = if null t then         go [] ws
                                                 else mkk t : go [] ws
                      | otherwise    = go (w:t) ws
          mkk = T.intercalate space . reverse

  -------------------------------------------------------------------------
  -- Keyword and its frequency and degree
  -------------------------------------------------------------------------
  type WordFreq  = (Text,Double,Double)

  -------------------------------------------------------------------------
  -- Map of Text and frequency,degree
  -------------------------------------------------------------------------
  type ScoreMap  = Map Text (Double,Double)

  -------------------------------------------------------------------------
  -- To calculate the scores we map 'kwScore' on all phrases
  -------------------------------------------------------------------------
  kwScores :: ScoreMap -> String -> [Text] -> [WordScore]
  kwScores m nsp = map (kwScore m nsp)

  -------------------------------------------------------------------------
  -- The keyword score is the sum of the individual scores 
  -- of all words contained in the keyword.
  -- The score per word is computed as (d+f)/f.
  -------------------------------------------------------------------------
  kwScore :: ScoreMap -> String -> Text -> WordScore
  kwScore m nsp s = let ws = splitWords nsp 0 s
                     in (s,sum $ map findScore ws)
    where findScore w = case M.lookup w m of
                          Nothing    -> 0
                          Just (f,d) -> (d+f) / f
    
  -------------------------------------------------------------------------
  -- We compute the word score folding 'M.insert' on all keywords
  -- and computing f as f+1 for each instance of the word
  --     and       d as d+d for each instance of the word
  -------------------------------------------------------------------------
  wordScores :: String -> [Text] -> ScoreMap 
  wordScores nsp = foldl' score M.empty . foldl' (wordScore nsp) []
    where score m (x,f,d) = M.insertWith add x (f,d) m
          add (f1,d1) (_,d2) = (f1+1,d1+d2)

  -------------------------------------------------------------------------
  -- Computing the keyword score as the number of words in the keyword.
  -- The addition of frequency (f+1) and degree (d+d) is folded on
  -- the table of all keywords.
  -------------------------------------------------------------------------
  wordScore :: String -> [WordFreq] -> Text -> [WordFreq]
  wordScore nsp wf s = let ws = splitWords nsp 0 s
                           f  = fromIntegral $ length ws
                           d  = f-1
                        in foldl' (inswf d) wf ws

  inswf :: Double -> [WordFreq] -> Text -> [WordFreq]
  inswf d [] s = [(s,1,d)]
  inswf d' ((w,f,d):ws) s | w == s    = (w,f+1,d'+d):ws
                          | otherwise = (w,f,d) : inswf d' ws s 
  
  -------------------------------------------------------------------------
  -- To compute keyword scores we use a wrapped wSplitter.
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
  splitWords :: String -> Int -> Text -> [Text]
  splitWords nsp m s = filter flt $ wSplitter nsp s
    where flt w = T.compareLength w m == GT &&
                  not (T.null w) &&
                  not (numeric w)

  -------------------------------------------------------------------------
  -- Simple definition of what is a number.
  -- There may be better definitions, though.
  -------------------------------------------------------------------------
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

