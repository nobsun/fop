-- ref.) https://arxiv.org/pdf/2110.01111.pdf
module ICBICSort where

import Debug.Trace (trace)

debug = True

tracing :: Show a => (a -> b) -> a -> b
tracing f x = if debug then trace (show x) (f x) else f x

-- I Can't Believe It Can Sort Algorithm.
icbics :: (Show a, Ord a) => [a] -> [a]
icbics xs = tracing go ([], xs)
  where go (xs, []) = xs
        go xys      = tracing go (swapper xys)

swapper :: (Show a, Ord a) => ([a], [a]) -> ([a], [a])
swapper (xs, y:ys) = (zs++[w], ws)
  where (z, zs) = swp (y:xs)
        (w, ws) = swp (z:ys)

swp :: Ord a => [a] -> (a, [a])
swp (x:xs) = case span (<x) xs of
  (xs',  []) -> (x, xs)
  (xs', ys') -> (z, xs'++[x]++zs)
    where (z, zs) = swp ys'

sample = [1,3,2,5,4,7,6,0]
