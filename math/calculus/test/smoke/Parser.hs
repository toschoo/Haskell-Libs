module Parser
where

  import           Math.Calculus.Function
  import           Data.Attoparsec.Text hiding (take, takeTill)
  import qualified Data.Attoparsec.Text as A (take, takeTill)
  import           Control.Applicative ((<|>), (<$>))
  import           Control.Monad (when)

  data Op = Op
  data Term = Sym Char | Formula Term Op Term

  term :: Context -> Parser Term
  term ctx = try (
               formula <|>
               symbol)

  symbol :: Parser Term
  symbol = Sym <$> (char     'a'
                    <|> char 'b'
                    <|> char 'c')

  formula :: Context -> Parser Term
  formula ctx = do 
    t1 <- symbol <|> variable ctx
    op <- operator
    t2 <- formula ctx -- try (formula ctx <|> symbol) -- term ctx)
    return $ Formula t1 op t2

  operator :: Parser Op
  operator = Op <$> char '+'

