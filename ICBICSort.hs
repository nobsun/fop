-- ref.) https://arxiv.org/pdf/2110.01111.pdf
module ICBICSort where

import Debug.Trace (trace)

debug = True

tracing :: Show a => a -> a
tracing wk = if debug then trace (show wk) wk else wk

-- I Can't Believe It Can Sort Algorithm.
icbics :: (Show a, Ord a) => [a] -> [a]
icbics xs = go ([], xs)
  where
    go (xs, []) = xs
    go xys      = go (swapper xys)

swapper :: (Show a, Ord a) => ([a], [a]) -> ([a], [a])
swapper (xs, y:ys) = tracing tmp
  where
    tmp = (zs++[w], ws)
    (z, zs) = swp (y:xs)
    (w, ws) = swp (z:ys)

swp :: Ord a => [a] -> (a, [a])
swp (x:xs) = case span (<x) xs of
  (xs',  []) -> (x, xs)
  (xs', ys') -> (z, xs'++[x]++zs)
    where (z, zs) = swp ys'

sample = [1,3,2,5,4,7,6,0]
