module Euler.P64_66 (p64, p65) where

import Euler.Util
import Data.Char
import Data.Ratio
import Control.Monad.State

compute :: Integer -> [Integer]
compute root = evalState (cont (root, 0, 1)) []
    where
        cont :: (Integer, Integer, Integer) -> State [Integer] [Integer]
        cont (r, n, d)
            | d == 0 = get
            | d == 1 && n /= 0 = update >> get
            | otherwise = update >> cont (r, -a, (r - a ^ (2 :: Integer)) `div` d)
                where
                    m = (truncate (sqrt (fromIntegral r) :: Double) + n) `div` d
                    a = n - d * m
                    update = modify $ \s -> s ++ [m]

evalFraction :: [Integer] -> Ratio Integer
evalFraction = foldr cont 0
    where
        cont x 0 = x % 1
        cont x y = (x % 1) + (1 / y)

p64 :: Solution
p64 input = show . length $ filter odd $ map (subtract 1 . length . compute) [2..limit]
    where
        limit = read input :: Integer

p65 :: Solution
p65 input = show.sum $ map digitToInt.show.numerator.evalFraction $ take limit e
    where
        limit = read input :: Int
        e = [2, 1] ++ concat [[n, 1, 1] | n <- [2,4..] :: [Integer]]

