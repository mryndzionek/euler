module Main where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

import Euler.Util

p3 :: Problem
p3 input = show $ maximum $ pfactors (read input)

p4 :: Problem
p4 input = show $ maximum $ filter isPalindrome numbers
    where
    isPalindrome x = let s = show x
                     in
                     s == reverse s
    numbers = [x*y | x <- [beg..end], y <- [x..end]] :: [Int]
    size = read input :: Int
    beg = 10 ^ (size - 1)
    end = (10 ^ size) - 1

p5 :: Problem
p5 input = show $ reduce . lp . nub . concat $ map (count . pfactors) [2..size]
    where
    size = read input :: Int
    reduce :: [(Int, Int)] -> Int
    reduce = product . map (uncurry (^))
    lp a = Map.toList $ foldr f Map.empty a
        where
        f (x, y) m = case Map.lookup x m of
                     Nothing -> Map.insert x y m
                     Just _  -> Map.adjust (max y) x m

p6 :: Problem
p6 input = show $ (s * s) - sum [x*x | x <- [1..size]]
    where
    size = read input :: Int
    s = sum [1..100]

p7 :: Problem
p7 input = show $ primes !! upper
    where
    upper = read input - 1 :: Int

p8 :: Problem
p8 input = show $ maximum $ unfoldr grow digits
    where
    digits = map digitToInt $ (concat . lines) input
    grow seed = case splitAt 13 seed of
                     (_, [])  -> Nothing
                     (xs, _)  -> Just (product xs, drop 1 seed)

p9 :: Problem
p9 input = show $ reduce [[a, b, c] | a <- [1..sum'],
                                      b <- [1..sum' - a],
                                      let c = sum' - a - b,
                                      a*a + b*b == c*c]
    where
    reduce = product.head
    sum' = read input :: Int
    

p10 :: Problem
p10 input = show $ sum (takeWhile (< size) primes)
    where
    size = read input :: Int

p11 :: Problem
p11 input = show $ maximum $ map (\x -> (product <$> x, x)) g
    where
    matrix = map (map read . splitOn " ") $ lines input
    g = concat [groups matrix (x, y) | x <- [0..19], y <- [0..19]]
    groups :: [[Int]] -> (Int, Int) -> [Maybe [Int]]
    groups m p = map (mapM (lu m)) $ indexes p
    indexes (x, y) = map (\(a, b) -> zip (itr4 x a) (itr4 y b)) succ'
    succ' :: [(Int -> Int, Int -> Int)]
    succ' = [((+1), id), ((+1), (+1)),
              (id, (+1)), (flip (-) 1, (+1)),
              (flip (-) 1, id), (flip (-) 1, flip (-) 1),
              (id, flip (-) 1), ((+1), flip (-) 1)]
    itr4 x f = take 4 $ iterate  f x
    lu :: [[Int]] -> (Int, Int) -> Maybe Int
    lu a (x, y) = safeLu y a >>= safeLu x

p12 :: Problem
p12 input = show $ snd . head $ dropWhile ((< size) . fst) (map cp triangle)
    where
    size = read input :: Int
    cp x = (length $ divisors x, x)
    triangle :: [Int]
    triangle = [sum [1..x] | x <- [1..]]

p13 :: Problem
p13 input = let numbers = map read $ lines input :: [Integer]
            in take 10 $ show.sum $ numbers

p14 :: Problem
p14 input = show.snd.maximum $ map (\x -> ((length.collatz) x, (head.collatz) x)) [x | x <- [1..size]]
    where
    size = read input - 1:: Int
    collatz n = (takeWhile (>1) $ iterate next n) ++ [1]
    next x
        | even x  = x `div` 2
        | odd  x  = 3 * x + 1
        | otherwise = undefined

problems :: [(String, Problem, IO String)]
problems = [
    ("Problem 3", p3, return"600851475143"),
    ("Problem 4", p4, return "3"),
    ("Problem 5", p5, return "20"),
    ("Problem 6", p6, return "100"),
    ("Problem 7", p7, return "10001"),
    ("Problem 8", p8, readFile "inputs/p8.txt"),
    ("Problem 9", p9, return "1000"),
    ("Problem 10", p10, return "2000000"),
    ("Problem 11", p11, readFile "inputs/p11.txt"),
    ("Problem 12", p12, return "500"),
    ("Problem 13", p13, readFile "inputs/p13.txt"),
    ("Problem 14", p14, return "1000000")]

main :: IO ()
main = do
  mapM_ printProblem problems
