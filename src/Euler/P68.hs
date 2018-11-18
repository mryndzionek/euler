module Euler.P68 (p68) where

import Euler.Util
import Control.Monad.State

rings :: [[[Integer]]]
rings = do
    [a, b, c, d, e, f, g, h, i, j] <- nPerms 10 [1..10]
    let ring = [[a, b, c],
                [d, c, e],
                [f, e, g],
                [h, g, i],
                [j, i, b]]
    guard $ minimum (map head ring) == a
    guard $ all (== head (map sum ring)) (tail (map sum ring))
    return ring

p68 :: () -> String
p68 _ = maximum $ filter ((==16).length) $ map toString rings
    where
        toString = concatMap show . concat

