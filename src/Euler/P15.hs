{-# LANGUAGE DeriveFunctor #-}
module Euler.P15 (p15, p15_brute) where

import Data.List
import Data.Fix

-- This brute force solution using F-algebras
-- Just for fun :)

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

data PathsF a = NodeF (Int, Int) [a]
  deriving Functor

coalg :: Int -> Coalgebra PathsF (Int, Int)
coalg s (x, y) = 
    NodeF (x, y) steps
    where
    steps = filter (\(x', y') -> x' <= s && y' <= s) [(x + 1, y), (x, y + 1)]

countAlg :: Algebra PathsF Int
countAlg (NodeF _ []) = 1
countAlg (NodeF _ lst) = sum lst

paths :: Int -> Int
paths size = hylo countAlg (coalg size) (0, 0)

p15_brute :: String -> Int
p15_brute input = paths size
    where
    size = read input :: Int

-- This is more efficient solution

p15 :: Int -> Integer
p15 input = last.last $ grid
    where
    size = 1 + input
    grid = take size $ iterate next (replicate size 1) :: [[Integer]]
    next row = unfoldr grow (0, row)
    grow (s, r:rs) = Just (s + r, (s + r,rs))
    grow (_, []) = Nothing
