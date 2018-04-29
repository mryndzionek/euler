{-# LANGUAGE DeriveFunctor #-}
module Euler.P15 (p15) where

import Data.Fix
import Euler.Util

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

p15 :: Problem
p15 input = show $ paths size
    where
    size = read input :: Int
