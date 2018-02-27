module Derivative where

data Expression = Number Double
                | Variable Symbol
                | Sum Expression Expression
                | Product Expression Expression deriving(Eq, Show)

type Symbol = String

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
