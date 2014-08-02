module Main
where

  import           System.Environment
  import qualified Data.Attoparsec.Text as A 
  import qualified Data.Text as T

  import           Math.Calculus.Parser
  import           Math.Calculus.Function

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      []  -> error "Ready!"
      [s] -> let (r, t) = parse s
              in putStrLn $ "Result: " ++ show r ++ "\n" ++
                            "Rest  : " ++ T.unpack t
      _   -> error "?????????"

  parse :: String -> (Term, T.Text)
  parse s = 
    case A.parse (term emptyCtx) (T.pack s) of
      x@(A.Partial _) -> case A.feed x T.empty of
                           A.Done t r -> (r, t)
                           _          -> error "on feed!"
      A.Done t r      -> (r, t)
      A.Fail _ _ e    -> error e
