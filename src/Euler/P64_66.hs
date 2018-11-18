module Euler.P64_66 (p64, p65, p66) where

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

p64 :: Integer -> Int
p64 limit = length $ filter odd $ map (subtract 1 . length . compute) [2..limit]

p65 :: Int -> Int
p65 limit = sum $ map digitToInt.show.numerator.evalFraction $ take limit e
    where
        e = [2, 1] ++ concat [[n, 1, 1] | n <- [2,4..] :: [Integer]]

p66 :: Integer -> Integer
p66 limit = snd.maximum $ zip (map findX candidates) candidates
    where
        candidates = filter (not . isPerfectSquare) ([2..limit] :: [Integer])
        isPerfectSquare val = val == sqrf * sqrf
            where
                sqrf = floor ((sqrt.fromIntegral) val :: Double)
        findX d = numerator.head $ filter isSolution (approx d)
            where
                isSolution x = a*a - d*b*b == 1
                    where
                        a = numerator x
                        b = denominator x
        approx root = [evalFraction (take x terms) | x <- [1..]]
            where
                as = compute root
                terms = head as : cycle (tail as)

