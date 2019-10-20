module Hylo where

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

fold :: (Either a [b] -> b) -> Tree a -> b
fold f t = case t of
  Leaf x -> f (Left x)
  Node ts -> f (Right (map (fold f) ts))

unfold :: (b -> Either a [b]) -> b -> Tree a
unfold g x = case g x of
  Left y -> Leaf y
  Right xs -> Node (map (unfold g) xs)

hylo f g x = case g x of
  Left y -> f (Left y)
  Right xs -> f (Right (map (hylo f g) xs))
