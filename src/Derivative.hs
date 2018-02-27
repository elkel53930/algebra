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
deriv (Sum e1 e2) s = makeSum (deriv e1 s) (deriv e2 s)
deriv (Product e1 e2) s =
  makeSum (makeProduct e1 $ deriv e2 s )
          (makeProduct e2 $ deriv e1 s )

makeSum :: Expression -> Expression -> Expression
makeSum e1 e2 =
  case (isZero e1, isZero e2) of
    (True, True) -> Number 0
    (True, False) -> e2
    (False, True) -> e1
    (False, False) -> Sum e1 e2

makeProduct :: Expression -> Expression -> Expression
makeProduct e1 e2 =
  case isZero e1 || isZero e2 of
    True -> Number 0
    False -> Product e1 e2

isZero :: Expression -> Bool
isZero (Number x) = x == 0
isZero (Variable _) = False
isZero (Sum e1 e2) = (isZero e1) && (isZero e2)
isZero (Product e1 e2) = (isZero e1) || (isZero e2)
