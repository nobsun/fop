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

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = fold (Leaf . f) Node

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

mkNexus :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus f g = label . extractL . until singleL (stepL g) . initialL f

initialL :: ([a] -> b) -> [a] -> Layer (LTree b)
stepL :: ([b] -> b) -> Layer (LTree b) -> Layer (LTree b)
singleL :: Layer (LTree b) -> Bool
extractL :: Layer (LTree b) -> LTree b

-- common

type Layer a = [a]

wrap :: a -> [a]
wrap x = [x]

-- h = split

group :: [a] -> [[a]]
group [] = []
group (x:y:xs) = [x,y]:group xs

initialL f = map (lleaf f . wrap)
stepL g = map (lnode g) . group
singleL = single
extractL = head

-- h = isegs
{-
group :: [a] -> [[a]]
group [x] = []
group (x:y:xs) = [x,y]:group (y:xs)
-}
-- h = minors
{--
group :: [a] -> [[a]]
group [x] = []
group (x:xs) = map (bind x) xs ++ group xs
  where bind x y = [x,y]
--}
