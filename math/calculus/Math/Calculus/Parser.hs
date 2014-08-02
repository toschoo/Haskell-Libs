module Math.Calculus.Parser
where

  import           Math.Calculus.Function
  import           Data.Attoparsec.Text hiding (take, takeTill)
  import qualified Data.Attoparsec.Text as A (take, takeTill)
  import           Control.Applicative ((<|>), (<$>))
  import           Control.Monad (when)

  term :: Context -> Parser Term
  term ctx = try (
               formula ctx <|>
               fun ctx     <|>
               real        <|>
               symbol      <|>
               variable ctx)

  real :: Parser Term
  real = Real <$> double

  symbol :: Parser Term
  symbol = Sym <$> (char     'a'
                    <|> char 'b'
                    <|> char 'c'
                    <|> char 'd')

  variable :: Context -> Parser Term
  variable ctx = do
    c <- (char     'x'
          <|> char 'y'
          <|> char 'z')
    case getVal c ctx of
      Nothing -> return $ Var c
      Just t  -> return t

  fun :: Context -> Parser Term
  fun ctx = do
    f <- (char 'f'
          <|> char 'g'
          <|> char 'h')
    case getFunDef f ctx of
      Nothing -> fail $ concat ["Function '", [f], "' not defined"]
      Just d  -> return $ Fun f d

  formula :: Context -> Parser Term
  formula ctx = do 
    o1 <- (char '(' <|> return ' ')
    t1 <- case o1 of
            '(' -> do t <- term ctx
                      _ <- char ')'
                      return t
            _   -> symbol <|> variable ctx
    op <- operator
    o2 <- (char '(' <|> return ' ')
    t2 <- case o2 of
            '(' -> do t <- term ctx
                      _ <- char ')'
                      return t
            _   -> term ctx
    if o1 == ' ' && o2 == ' ' && (prio op > fPrio t2)
      then return $ reorganise t1 op t2
      else return $ Formula t1 op t2 

  {-
  formula :: Context -> Parser Term
  formula ctx = do 
    t1 <- symbol <|> variable ctx
    op <- operator
    t2 <- term ctx
    return $ Formula t1 op t2
  -}

  reorganise :: Term -> Op -> Term -> Term
  reorganise t1 o1 (Formula t2 o2 t3) = Formula (Formula t1 o1 t2) o2 t3
  reorganise t1 o t2 = Formula t1 o t2

  operator :: Parser Op
  operator = do
    c <- (char '+'
          <|> char '-'
          <|> char '*'
          <|> char '/'
          <|> char '^'
          <|> char 'v')
    case c of
      '+' -> return Add
      '-' -> return Sub
      '*' -> return Mul
      '/' -> return Div
      '^' -> return Pow
      'v' -> return Root

  prio :: Op -> Int
  prio Add  = 1
  prio Sub  = 1
  prio Mul  = 2
  prio Div  = 2
  prio Pow  = 3
  prio Root = 3

  fPrio :: Term -> Int
  fPrio (Formula _ o _) = prio o
  fPrio _               = 5


