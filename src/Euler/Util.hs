module Euler.Util where

import Data.List
import qualified Data.Map.Strict as Map

type Solution = String -> String

primes :: [Int]
primes = 2 : filter (null . tail . pfactors) [3,5..]

pfactors :: Int -> [Int]
pfactors n = factor n primes
  where
    factor n' (p:ps) 
        | p*p > n'        = [n']
        | n' `mod` p == 0 = p : factor (n' `div` p) (p:ps)
        | otherwise       =     factor n' ps
    factor _ [] = undefined

divisors :: Integral a => a -> [a]
divisors n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where
     limit = (floor.sqr) n
     sqr :: Integral a => a -> Double
     sqr = sqrt.fromIntegral

fibs :: [Integer]
fibs = unfoldr (\(a, b) -> Just (a, (b, a + b))) (1, 2)

safeLu :: Int -> [a] -> Maybe a
safeLu i a
    | (i >= 0) && (length a > i) = Just (a !! i)
    | otherwise = Nothing

count :: (Ord k, Num a, Foldable t) => t k -> [(k, a)]
count a = Map.toList $ foldr f Map.empty a
    where
    f k m = case Map.lookup k m of
        Nothing -> Map.insert k 1 m
        Just _  -> Map.adjust (+1) k m

printSolution :: (Int, Solution, IO String) -> IO ()
printSolution (number, problem, input) = do
    i <- input
    putStrLn $ "Problem " ++ show number ++ ": " ++ problem i
