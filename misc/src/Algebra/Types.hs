module Algebra.Types
where

  import Data.List (find, delete, sort, intercalate)
  import Debug.Trace (trace)

  type Symbol = String

  data Term = P Integer [Symbol]
    deriving (Show,Eq)

  instance Ord Term where
    compare (P _ as) (P _ bs) = compare as bs

  type Number = [Term]

  zero,one,two,ten :: Number
  zero = [P  0 []]
  one  = [P  1 []]
  two  = [P  2 []]
  ten  = [P 10 []]

  pretty :: Number -> String
  pretty [] = "0"
  pretty n  = concatMap prettyTerm n

  prettyTerm :: Term -> String
  prettyTerm (P 0 _ ) = "0"
  prettyTerm (P i []) = show i
  prettyTerm (P i xs) = let s | i >= 0    = "+"
                              | otherwise = "" 
                            f | i == 1    = ""
                              | i == -1   = "-"
                              | otherwise = show i
                         in s ++ f ++ (concat xs)

  add :: Term -> Term -> Number
  add (P ia as) (P ib bs) | sort as == sort bs = [P (ia+ib) as]
                          | otherwise          = [P ia as, P ib bs]

  addN :: Number -> Number -> Number
  addN [] b = b
  addN a [] = a
  addN (a:as) bs = addN as (sort $ insert a bs)

  insert :: Term -> Number -> Number
  insert x [] = [x]
  insert x (z:zs) = case add x z of
                      [P 0 _] -> zs
                      [y]     -> y : zs
                      _       -> z : insert x zs

  mul :: Term -> Term -> Number
  mul (P ia as) (P ib bs) = [P (ia*ib) (sort $ as++bs)]

  mulN :: Number -> Number -> Number
  mulN [] b = []
  mulN a [] = []
  mulN [P 0 _] _ = []
  mulN _ [P 0 _] = []
  mulN (a:as) bs = addN (concatMap (mul a) bs) (mulN as bs)
  
  isZero :: Term -> Bool
  isZero (P 0 _) = True

  test :: IO ()
  test = do
    let a = [P 1 ["a"]]
    let b = [P 1 ["b"]]
    let c = addN a b
    let d = addN a c
    let e = mulN c c
    putStrLn $ pretty c
    putStrLn $ pretty d
    putStrLn $ pretty e
    let f = [P (-1) ["b"]]
    let g = mulN e f
    let h = addN a f
    let i = mulN h h
    let j = mulN c h
    putStrLn $ pretty g
    putStrLn $ pretty h
    putStrLn $ pretty i
    putStrLn $ pretty j
    putStrLn $ pretty $ mulN [P 0 []] j

