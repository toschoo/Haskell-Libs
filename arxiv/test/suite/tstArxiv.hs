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

  import Network.Api.Arxiv
  import Common

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

  main :: IO ()
  main = putStrLn "Arxiv Test Suite" >> checkAll

  prpDslSimple :: Bool
  prpDslSimple = 
    let au = Exp $ Au ["Knuth"]
        t1 = Exp $ Ti ["The Art of Computer Programming"]
        t2 = Exp $ Ti ["Concrete Mathematics"]
        x  = au /*/ (t1 /+/ t2)
        q  = "au:Knuth+AND+" ++
             "%28ti:%22The+Art+of+Computer+Programming%22+" ++
             "OR+ti:%22Concrete+Mathematics%22%29" 
     in trace (exp2str x) $ exp2str x == q

  prpParse0 :: Bool
  prpParse0 = let q = "au:Knuth+AND" ++
                      "+%28ti:%22The+Art+of+Computer+Programming%22" ++
                      "+OR" ++
                      "+ti:+%22Concrete+Mathematics%22%29"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> trace (show x) True

  prpParse1 :: Bool
  prpParse1 = let q = "au:Knuth"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q

  prpParse2 :: Bool
  prpParse2 = let q = "ti:The+Art+of+Computer+Programming"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q

  prpParse3 :: Bool
  prpParse3 = let q = "ti:%22The+Art+of+Computer+Programming%22"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q

  prpParse4 :: Bool
  prpParse4 = let q = "au:Knuth+AND+ti:The+Art+of+Computer+Programming"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q

  prpParse5 :: Bool
  prpParse5 = let q = "au:Knuth+AND+ti:%22The+Art+of+Computer+Programming%22"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q

  prpParse6 :: Bool
  prpParse6 = let q = "au:Knuth+AND" ++
                      "+ti:%22The+Art+of+Computer+Programming%22" ++
                      "+OR" ++
                      "+ti:%22Concrete+Mathematics%22"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x /= q
                               -- parentheses missing in q!

  prpParse7 :: Bool
  prpParse7 = let q = "au:Knuth+AND" ++
                      "+%28ti:%22The+Art+of+Computer+Programming%22" ++
                      "+OR" ++
                      "+ti:%22Concrete+Mathematics%22%29"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q 

  prpParse8 :: Bool
  prpParse8 = let q = "%28au:Knuth+AND" ++
                      "+ti:%22The+Art+of+Computer+Programming%22%29" ++
                      "+OR" ++
                      "+ti:%22Concrete+Mathematics%22"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == q

  prpParse9 :: Bool
  prpParse9 = let q = "au:Knuth+AND" ++
                      "+(ti:\"The+Art+of+Computer+Programming\"" ++
                      "+OR" ++
                      "+ti:%22Concrete+Mathematics%22)"
               in case parseQuery q of
                    Left  e -> trace e $ False
                    Right x -> exp2str x == preprocess q 

  prpParseAny :: Expression -> Bool
  prpParseAny e = let q = exp2str e
                   in case parseQuery q of
                        Left  r -> trace ("\n" ++ r ++ ":\n" ++
                                             show e ++ ":\n" ++ q) False
                        Right x -> exp2str x == q

  preprocessx :: String -> String
  preprocessx = replace " " "+"   . 
                replace "(" "%28" . 
                replace ")" "%29" . 
                replace "\"" "%22"

  prpPre1 :: Bool
  prpPre1 = let s = "hello+world" in s == preprocess s

  prpPre2 :: Bool
  prpPre2 = preprocess "(hello) world" == "%28hello%29+world"

  prpPre3 :: String -> Bool
  prpPre3 s = preprocess s == preprocessx s

  type Soup = Tag String

  prpCountEntries :: String -> Bool
  prpCountEntries = withTags $ \ts -> 
    let t = totalResults ts
     in t == sum (forEachEntry ts $ const (1::Int))

  prpCountEntriesM :: String -> Property
  prpCountEntriesM = withMonadicTags $ \ts -> do 
    let t = totalResults ts
    r <- sum <$> run (forEachEntryM ts (\_ -> return (1::Int)))
    assert (r == t)

  prpAuthors :: String -> Bool
  prpAuthors = withTags $ \ts ->
    and $ forEachEntry ts (\e ->
      let aus = getAuthors e
          ns  = getAuthorNames e
       in ns == map auName aus)

  prpYear :: String -> Bool
  prpYear = withTags $ \ts ->
    and $ forEachEntry ts (\e ->
      let p = getPublished e
          y = getYear      e
       in y == takeWhile (/= '-') p)

  prpPdfLink :: String -> Bool
  prpPdfLink = withTags $ \ts ->
    and $ forEachEntry ts (\e ->
      case getPdfLink e of
        Nothing -> False
        Just p  -> case getLinks e of
                     [] -> False
                     ls -> p `elem` ls)

  prpPdf :: String -> Bool
  prpPdf = withTags $ \ts ->
    and $ forEachEntry ts (\e ->
      case getPdfLink e of
        Nothing -> False
        Just p  -> case getPdf e of
                     "" -> False
                     s  -> s == lnkHref p)

  prpPrimaryCat :: String -> Bool
  prpPrimaryCat = withTags $ \ts ->
    and $ forEachEntry ts (\e ->
      case getPrimaryCategory ts of
        Nothing -> False
        Just p  -> case getCategories ts of
                     [] -> False
                     cs -> p `elem` cs)

  -- visual tests only!
  prpId :: String -> Property
  prpId = visualTest getId

  -- visual tests only!
  prpUpdated :: String -> Property
  prpUpdated = visualTest getUpdated

  -- visual tests only!
  prpPublished :: String -> Property
  prpPublished = visualTest getPublished

  -- visual tests only!
  prpTitle :: String -> Property
  prpTitle = visualTest getTitle

  -- visual tests only!
  prpSummary :: String -> Property
  prpSummary = visualTest getSummary

  -- visual tests only!
  prpComment :: String -> Property
  prpComment = visualTest getComment

  -- visual tests only!
  prpJournal :: String -> Property
  prpJournal = visualTest getJournal

  -- visual tests only!
  prpDoi :: String -> Property
  prpDoi = visualTest getDoi

  -- links
  prpLinks :: String -> Property
  prpLinks = visualTest getLinks

  -- getCategories
  prpCategories :: String -> Property
  prpCategories = visualTest getCategories

  visualTest :: Show a => ([Soup] -> a) -> String -> Property
  visualTest sut = withMonadicTags $ \ts -> do
    run $ forEachEntryM_ ts (print . sut)
    assert True

  withTags :: ([Soup] -> r) -> String -> r
  withTags f = f . parseTags 

  withMonadicTags :: ([Soup] -> PropertyM IO r) -> String -> Property
  withMonadicTags f = monadicIO . f . parseTags 

  checkAll :: IO ()
  checkAll = do
    txt <- readFile "test/suite/result1.feed"
    let good = "OK. All Tests passed."
    let bad  = "Bad. Some Tests failed."
    r <- deepCheck prpDslSimple ?>
         deepCheck prpParse1    ?>
         deepCheck prpParse2    ?>
         deepCheck prpParse3    ?>
         deepCheck prpParse4    ?> 
         deepCheck prpParse5    ?>
         deepCheck prpParse6    ?>
         deepCheck prpParse7    ?>
         deepCheck prpParse8    ?>
         deepCheck prpParse9    ?>
         deepCheck prpParseAny  ?>
         oneCheck  prpPre1      ?>
         oneCheck  prpPre2      ?>
         deepCheck prpPre3      ?>
         oneCheck  (prpCountEntries txt)  ?>
         oneCheck  (prpCountEntriesM txt) ?>
         oneCheck  (prpAuthors txt)       ?>
         oneCheck  (prpPdfLink    txt)    ?>
         oneCheck  (prpPdf        txt)    ?>
         oneCheck  (prpPrimaryCat txt)    ?>
         oneCheck  (prpId      txt)       ?>
         oneCheck  (prpYear    txt)       ?>
         oneCheck  (prpUpdated txt)       ?>
         oneCheck  (prpPublished  txt)    ?>
         oneCheck  (prpTitle      txt)    ?>
         oneCheck  (prpSummary    txt)    ?>
         oneCheck  (prpComment    txt)    ?>
         oneCheck  (prpJournal    txt)    ?>
         oneCheck  (prpDoi        txt)    ?> 
         oneCheck  (prpLinks      txt)    ?> 
         oneCheck  (prpCategories txt)    
    case r of
      Success {}-> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure


