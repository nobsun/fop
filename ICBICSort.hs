module ICBICSort where

import Debug.Trace (trace)

debug = True

tracing :: Show a => a -> a
tracing wk = if debug then trace (show wk) wk else wk

sample = [1,3,2,5,4,7,6,0]

-- I Can't Believe It Can Sort Algorithm.
icbics :: (Ord a, Show a) => [a] -> [a]
icbics xs = go ([], xs)
  where
    go (xs, []) = xs
    go xys      = go (swapper xys)

swapper :: (Show a, Ord a) => ([a],  [a]) -> ([a], [a])
swapper (xs, y:ys) = tracing tmp
  where
    tmp = (zs++[w], ws)
    (z, zs) = swp (y:xs)
    (w, ws) = swp (z:ys)

swp :: Ord a => [a] -> (a, [a])
swp (x:xs) = case span (<x) xs of
  (xs',  []) -> (x, xs)
  (xs', ys') -> let (z, zs) = swp ys'
                in  (z, xs'++[x]++zs)
