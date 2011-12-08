module Main where

  import System.Environment
  import Graph
  import Visual
  import qualified Data.IOMap as T

  import Data.List
  import Data.Char

  str2list :: String -> [Int]
  str2list s = map toNum $ words s

  toNum :: String -> Int
  toNum w = 
     if numeric w then read w else error $ "Not a number: " ++ w

  numeric :: String -> Bool
  numeric "" = False
  numeric (c:cs) = if c == '-' 
                     then restN cs
                     else restN (c:cs)
    where restN = all isDigit 

  data Op = Nop
          | Del Int
          | Ins Int

  str2ops :: String -> [Op]
  str2ops "" = []
  str2ops s  = let (o, r) = str2op s
                in o : str2ops r

  str2op :: String -> (Op, String)
  str2op "" = (Nop, "")
  str2op s  = let ws     = words s
                  (o,r)  = if null ws then error $ "No operation in: " ++ s
                           else (head ws, tail ws)
                  v      = if null r then error $ "No value in: " ++ s
                           else head r
                  i      = toNum v
               in case o of
                    "ins" -> (Ins i, unwords $ tail r)
                    "del" -> (Del i, unwords $ tail r)
                    _     -> error $ "not an op: " ++ o

  applyOp :: T.IOMap Int () -> Op -> IO ()
  applyOp t op  =
    case op of
      Ins i -> T.insert t upd i ()
      Del i -> T.delete t i
      _     -> return ()
    where upd _ = return

  main :: IO ()
  main = do
    args <- getArgs
    let (os, ns) = case args of
                     []    -> error "Nothing to sort"
                     [a]   -> ([], str2list a)
                     [a,b] -> (str2ops a, str2list b)
                     _     -> error "Don't know what to do with the second argument!"
    putStrLn (show ns)
    t  <- buildTree ns
    mapM_ (applyOp t) os
    ls <- T.tree2Lines t startPoint startLength 
    drawLines ls

  buildTree :: [Int] -> IO (T.IOMap Int ())
  buildTree ns = do
    t <- T.empty
    mapM_ (\k -> T.insert t upd k ()) ns
    return t
    where upd _ = return
