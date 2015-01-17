module Algebra.Types
where

  import Data.List (find, delete, sort, intercalate)
  import Debug.Trace (trace)

  type Symbol = String

  data Var = Const Int
           | Var Symbol
           | P Int [Var]
           | S [Var]
           | Neg Var
           -- | Inv Var
    deriving (Show)

  -- we need two notions of equality:
  -- 1) in the sense of Eq (equal counters on P)
  -- 2) in terms of same variable (counters on P may differ)
  instance Eq Var where
    (Const a) == (Const b) = a == b
    (Var   a) == (Var   b) = a == b
    (P a as ) == (P b bs)  = a == b && sort as == sort bs
    (S as)    == (S bs)    = sort as == sort bs
    (Neg a)   == (Neg b)   = a == b
    _         == _         = False 

  same :: Var -> Var -> Bool
  same (P _ as) (P _ bs) = sort as == sort bs
  same a b               = a == b

  instance Ord Var where
    compare (Const a) (Const b) = compare a b
    compare (Const _) _         = LT
    compare _ (Const _)         = GT
    compare (Var x) (Var y)     = compare x y
    compare (Var _) _           = GT
    compare _ (Var _)           = LT
    compare (P _ _) (S _)       = LT
    compare (S _) (P _ _)       = GT
    compare (P _ _) (P _ _)     = EQ
    compare (S _) (S _)         = EQ
    compare (Neg a) (Neg b)     = compare a b
    compare (Neg _) _           = LT
    compare _ (Neg _)           = GT

  -------------------------------------------------------------------------
  -- Multiplication
  -------------------------------------------------------------------------
  mul :: Var -> Var -> Var
  mul (Const 1) b     = b
  mul a (Const 1)     = a

  mul (Neg a) (Neg b) = mul a b
  mul (Neg a) b       = Neg (mul a b)
  mul a (Neg b)       = Neg (mul a b)

  mul (Const a) (Const b) = Const (a*b)
  mul (Const a) (Var b)   = P a [Var b]
  mul (Const a) (S bs)    = S (map (mul $ Const a) bs)
  mul (Const a) (P i bs)  = P (a*i) bs

  mul (Var a) (Const b)   = mul (Const b) (Var a) 
  mul (Var a) (Var b)     = P 1 [Var a, Var b] 
  mul (Var a) (S bs)      = S (map (mul (Var a)) bs)
  mul (Var a) (P ib bs)   = P ib (sort $ (Var a):bs)

  mul (S bs) (Const a)    = mul (Const a) (S bs)
  mul (S bs)  (Var a)     = mul (Var a) (S bs)
  mul (S as) (S bs)       = S (simplify $ concatMap (`combine` as) bs)
  mul (S bs) (P ia as)    = mul (P ia as) (S bs)

  mul (P ib bs) (Const a) = mul (Const a) (P ib bs)
  mul (P ib bs) (Var a)   = mul (Var a) (P ib bs)
  mul (P ia as) (P ib bs) = P (ia*ib) (sort $ as++bs)
  mul (P ia as) (S bs)    = S (simplify $ map (mul (P ia as)) bs)

  -------------------------------------------------------------------------
  -- Addition
  -------------------------------------------------------------------------
  add :: Var -> Var -> Var
  add (Const 0) b         = b
  add a (Const 0)         = a

  add (Neg a) (Neg b)       = Neg $ add a b
  add (Neg (Const a)) b     = add (Const (-a)) b
  add a (Neg (Const b))     = add a (Const (-b))
  add (Neg a) b | a == b    = Const 0
                | otherwise = S [b, Neg a]
  add b (Neg a) = add (Neg a) b

  add (Const a) (Const b) = Const (a+b)
  add (Const a) (Var b)   = S [Const a, Var b]
  add (Const a) (S bs)    = S (insert (Const a) bs)
  add (Const a) (P i bs)  = S [Const a, P i bs]

  add (Var b) (Const a)           = add (Const a) (Var b)
  add (Var a) (Var b) | a == b    = P 2 [Var a]
                      | otherwise = S [Var a, Var b]
  add (Var a) (S bs)              = S (simplify ((Var a) : bs))
  add (Var a) (P i bs)            = S [Var a, P i bs]
  
  add (S bs) (Const a)    = add (Const a) (S bs)
  add (S bs) (Var a)      = add (Var a) (S bs)
  add (S as) (P i bs)     = S ((P i bs):as)
  add (S as) (S bs)       = S (merge as bs)

  add (P i bs) (Const a)  = add (Const a) (P i bs)
  add (P i bs) (Var a)    = add (Var a) (P i bs)
  add (P i bs) (S as)     = add (S as) (P i bs)
  add (P ia as) (P ib bs) = S [P ia as, P ib bs]

  sub :: Var -> Var -> Var
  sub a b = add a (Neg b)

  combine :: Var -> [Var] -> [Var]
  combine a = map (mul a)

  insert :: Var -> [Var] -> [Var]
  insert x [] = [x]
  insert (Const i) ((Const j):xs) = insert (add (Const i) (Const j)) xs
  insert (Const i) (x:xs)         = x : insert (Const i) xs
  insert (Var   a) ((Var b):xs) | a == b = insert (P 2 [Var a]) xs
                                | otherwise = (Var b) : insert (Var a) xs
  insert (Var a) (x:xs) = x : insert (Var a) xs
  insert (S as) ((S bs):xs) | sort as == sort bs = insert (P 2 (as ++ bs)) xs
                            | otherwise = (S bs) : insert (S as) xs
  insert (S as) (x:xs)  = x : insert (S as) xs
  insert (P ia as) ((P ib bs):xs) | sort as == sort bs = insert (P (ia+ib) as) xs
                                  | otherwise          = (P ib bs) : insert (P ia as) xs
  insert (P ia as) (x:xs) = x : insert (P ia as) xs
  insert (Neg a) ((Neg b):xs) | same a b = insert (add (Neg a) (Neg b)) xs
  insert (Neg a) (x:xs) = x : insert (Neg a) xs

  merge :: [Var] -> [Var] -> [Var]
  merge [] zs = zs
  merge xs [] = xs
  merge (x:xs) zs = merge xs (insert x zs)

  simplify :: [Var] -> [Var] 
  simplify [] = []
  simplify (x:xs) = {- trace (show $ x:xs) $ -} let (i,ys) = del x xs 
                     in if i == 0 then     x : simplify ys
                                  else let (f,x') = case x of
                                                      Neg a -> (Neg,a)
                                                      _     -> (id,x)
                                           n = case x' of
                                                 P j as  -> P (j+i) as 
                                                 S as    -> P (i+1) as
                                                 Var a   -> P (i+1) [Var a]
                                                 Const j -> Const (i+j)
                                                 a       -> a
                                        in (f n) : simplify ys
    where del _ [] = (0,[])
          del (Const i) zs = case find isConst zs of
                               Nothing        -> (0,zs)
                               Just (Const j) -> let (y,ys) = del (Const j) (
                                                              delete (Const j) zs)
                                                  in (i+y, ys)
                               Just _         -> error "ouch!"
          del (P i as) zs = case find (same (P i as)) zs of
                              Nothing -> (0,zs)
                              Just (P j _) -> let (y,ys) = del (P j as) (delete (P j as) zs)
                                               in (j+y,ys)
                              Just _       -> error "ouch!"
          del z zs = case find (same z) zs of
                       Nothing -> (0,zs)
                       Just _  -> let (y,ys) = del z (delete z zs) in (1+y,ys)
          isConst (Const _) = True
          isConst _         = False

  pretty :: Var -> String
  pretty (Neg a)   = "-" ++ pretty a
  pretty (Const i) = show i
  pretty (Var s)   = s
  pretty (S as)    = intercalate "+" (map pretty as)
  pretty (P i as)  = let c | i /= 1    = show i 
                           | otherwise = ""
                      in c ++ (concatMap pretty as)
