module Derivative where

import Text.ParserCombinators.Parsec

data Expression = Number Double
                | Variable Symbol
                | Sum Expression Expression
                | Product Expression Expression deriving(Eq, Show)

type Symbol = String

derivExp :: Symbol -> String ->  Either String String
derivExp s src =
  let result = parse pExpression "" src
  in  case result of
        Right exp -> Right . oExpression . simplify $ deriv exp s
        Left  err -> Left $ show err

deriv :: Expression -> Symbol -> Expression
deriv (Number _) _ = Number 0
deriv (Variable v) s =
  if v == s
    then Number 1
    else Number 0
deriv (Sum e1 e2) s = Sum (deriv e1 s) (deriv e2 s)
deriv (Product e1 e2) s =
  Sum (Product e1 $ deriv e2 s )
      (Product e2 $ deriv e1 s )

isZero :: Expression -> Bool
isZero (Number x) = x == 0
isZero (Variable _) = False
isZero (Sum e1 e2) = (isZero e1) && (isZero e2)
isZero (Product e1 e2) = (isZero e1) || (isZero e2)

simplify :: Expression -> Expression
simplify e@(Sum e1 e2) =
  let
    s1 = simplify e1
    s2 = simplify e2
  in
    case (s1, s2) of
      (Number 0, er)         -> er
      (el, Number 0)         -> el
      (Number x1, Number x2) -> Number $ x1+x2
      otherwise -> Sum s1 s2
simplify e@(Product e1 e2) =
  let
    s1 = simplify e1
    s2 = simplify e2
  in
    case (s1, s2) of
      (Number 1, er)         -> er
      (el, Number 1)         -> el
      (Number 0, _)          -> Number 0
      (_, Number 0)          -> Number 0
      (Number x1, Number x2) -> Number $ x1*x2
      otherwise -> Product s1 s2
simplify e = e

pExpression :: Parser Expression
pExpression =   try pNumber
            <|> try pVariable
            <|> try pSum
            <|> try pProduct

pNumber :: Parser Expression
pNumber = do
  intPart <- many1 digit
  decPart <- option "" $ do
    char '.'
    decPart <- many1 digit
    return $ '.' : decPart
  return . Number $ read (intPart ++ decPart)

pVariable :: Parser Expression
pVariable = do
  s <- many1 $ letter <|> char '_'
  return $ Variable s

pSum :: Parser Expression
pSum = do
  char '('
  e1 <- pExpression
  char '+'
  e2 <- pExpression
  char ')'
  return $ Sum e1 e2

pProduct :: Parser Expression
pProduct = do
  char '('
  e1 <- pExpression
  char '*'
  e2 <- pExpression
  char ')'
  return $ Product e1 e2

oExpression :: Expression -> String
oExpression (Number x) = show x
oExpression (Variable s) = s
oExpression (Sum e1 e2) = concat["(", oExpression e1, "+", oExpression e2, ")" ]
oExpression (Product e1 e2) = concat["(", oExpression e1, "*", oExpression e2, ")" ]
