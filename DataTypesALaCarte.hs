{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | ref.) http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
module DataTypesALaCarte where
{--
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

render :: Expr -> String
render (Val x) = show x
render (Add x y) = "(" ++ render x ++ "+" ++ render y ++ ")"
--}

data Val e = Val Int
type IntExpr = Expr Val
data Add e = Add e e
type AddExpr = Expr Add
data Expr f = In (f (Expr f))

data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- val :: Int -> Expr Val
-- val x = In (Val x)

-- infixl 6 .+.
-- (.+.) :: Expr Add -> Expr Add -> Expr Add
-- x .+. y = In (Add x y)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj
val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 .+.
(.+.) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x .+. y = inject (Add x y)
