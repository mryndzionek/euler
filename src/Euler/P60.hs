module Euler.P60 (p60) where

import Control.Monad.State
import Data.Numbers.Primes

check :: Integer -> Integer -> Bool
check x y = all isPrime ([read $shows x $show y,
                          read $shows y $show x] :: [Integer])

choosePrime :: Integer -> StateT [Integer] [] Integer
choosePrime p = StateT (`choose` p)
    where
        choose s n = s >>= \v -> 
                     return (v, filter (check v) $ dropWhile (<=n) s)

solve :: StateT [Integer] [] [Integer]
solve = let choices = replicateM 5 choosePrime
        in sequence $ choices 3 

p60 :: Integer -> Integer
p60 limit = sum .head $ evalStateT solve (takeWhile (<= limit) primes)
