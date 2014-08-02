module Math.Calculus.Function
where

  data Op = Add | Sub | Mul | Div | Pow | Root 
    deriving (Show, Eq, Ord)

  data Term =  Real Double 
             -- | Comp Complex
             | Sym Char
             | Var Char
             | Formula Term Op Term
             | Fun Char Term
    deriving (Show, Eq)

  type F = Term  

  data Context = Ctx {
                   ctxFun :: [(Char, Term)],
                   ctxVar :: [(Char, Term)]
                 }
   deriving (Show, Eq)

  getVal :: Char -> Context -> Maybe Term
  getVal c = lookup c . ctxVar

  getFunDef :: Char -> Context -> Maybe Term
  getFunDef c = lookup c . ctxFun

  emptyCtx :: Context
  emptyCtx = Ctx [] []
 
