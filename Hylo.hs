module Hylo where

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts) = g (map (fold f g) ts)
  
unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x)
                 else Node (map (unfold p v h) (h x))

data LTree a = LLeaf a | LNode a [LTree a] deriving (Show, Eq)

fill :: (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)

lleaf f x = LLeaf (f x)
lnode g ts = LNode (g (map label ts)) ts

label (LLeaf x) = x
label (LNode x ts) = x

hylo :: ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo f g h = fold f g . mkTree h

mkTree h = unfold single id h

single [] = False
single (x:[]) = True
single (x:xs) = False

----------------------------------------------------------

split xs = [take n xs, drop n xs] where n = length xs `div` 2

----------------------------------------------------------
isegs xs = [init xs, tail xs]

recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

{-# RULES
  "recover/isegs" forall xs. recover (isegs xs) = xs
  #-}

----------------------------------------------------------
minors [x,y] = [[x],[y]]
minors (x:xs) = map (x:) (minors xs) ++ [xs]

----------------------------------------------------------

type Layer a = [a]

mkNexus :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus f g = label . extractL . until singleL (stepL g) . initialL f

wrap :: a -> [a]
wrap x = [x]

initialL :: ([a] -> b) -> [a] -> Layer (LTree b)
initialL f = map (lleaf f . wrap)

singleL :: Layer (LTree b) -> Bool
singleL = single

extractL :: Layer (LTree b) -> LTree b
extractL = head

stepL :: ([b] -> b) -> Layer (LTree b) -> Layer (LTree b)
stepL g = map (lnode g) . group

group :: [a] -> [[a]]
group [] = []
group (x:y:xs) = [x,y]:group xs
