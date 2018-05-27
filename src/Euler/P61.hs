module Euler.P61 (p61) where

import Euler.Util
import Control.Monad

p61 :: Solution
p61 input = show.sum.head $ do
    p3 <- gen (\n -> n * (n + 1) `quot` 2)
    p4 <- gen (\n -> n * n)
    guard $ matches [p3] p4
    p5 <- gen (\n -> (n * (3 * n - 1)) `quot` 2)
    guard $ matches [p3, p4] p5
    p6 <- gen (\n -> n * (2 * n - 1))
    guard $ matches [p3, p4, p5] p6
    p7 <- gen (\n -> (n * (5 * n - 3)) `quot` 2)
    guard $ matches [p3, p4, p5, p6] p7
    p8 <- gen (\n -> n * (3 * n - 2))
    guard $ matches [p3, p4, p5, p6, p7] p8
    p <- filter match (combinations 6 [p3, p4, p5, p6, p7, p8])
    guard $ cyclic p
    return p
    where
        limit = read input :: Integer
        gen f = dropWhile (<1000) $ takeWhile (<= limit) [f n | n <- [1..]]

check :: Integer -> Integer -> Bool
check x y = (x `quot` 100) == (y `rem` 100)

matches :: Foldable t => t Integer -> Integer -> Bool
matches ps p = any (`check` p) ps || any (check p) ps

match :: Integral b => [b] -> Bool
match ps = and $
            zipWith (==) (map (`rem` 100) ps)
                         (map (`quot` 100) $ drop 1 ps)

cyclic :: Integral b => [b] -> Bool
cyclic ps = match (ps ++ [head ps])
