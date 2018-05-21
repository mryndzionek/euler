module Euler.P51 (p51) where

import Euler.Util
import Control.Monad
import Data.List
import Data.Numbers.Primes
import qualified Data.Map.Strict as Map

p51 :: Solution
p51 input = show.maximum $ map (\(a, b) -> (length b, a, sort b)) $ gather $ solve upper lower
     where
    digits = read input :: Int
    upper = 10 ^ digits :: Int
    lower = 10 ^ (digits - 1) :: Int

gather :: (Ord k, Foldable t) => t (k, a) -> [(k, [a])]
gather a = Map.toList $ foldr f Map.empty a
    where
    f (p, d) m = case Map.lookup p m of
        Nothing -> Map.insert p [d] m
        Just _  -> Map.adjust (++[d]) p m

solve :: Int -> Int -> [(String, Char)]
solve upper lower = do
    candidate <- takeWhile (<upper) $ dropWhile (<lower) primes
    let string = show candidate
    (digit, count') <- count string
    p <- genPattern count' 3
    return (patternReplace digit string p, digit)

patternReplace :: Char -> String -> String -> String
patternReplace d s p = unfoldr (r d) (s, p)
    where
    r _ ([], _)  = Nothing
    r _ (s', []) = Just (head s', (tail s', []))
    r d' (s', p')
        | head s' == d' = case head p' of
                               '_' -> Just (head s', (tail s', tail p'))
                               _   -> Just (head p', (tail s', tail p'))
        | otherwise   = Just (head s', (tail s', p'))

genPattern :: Int -> Int -> [String]
genPattern n m = do
    p <- replicateM n "*_"
    guard $ m == length (filter (== '*') p)
    return p
