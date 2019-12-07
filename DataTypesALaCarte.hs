-- | ref.) http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
module DataTypesALaCarte where

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

render :: Expr -> String
render (Val x) = show x
render (Add x y) = "(" ++ render x ++ "+" ++ render y ++ ")"

