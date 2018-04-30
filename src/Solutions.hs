module Main where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

import Euler.P15
import Euler.Util

p1 :: Problem
p1 input = (show.sum) $ filter (multOf [3, 5]) [1..size - 1]
    where
    size = read input :: Int
    multOf ms x = any (\f -> f x == 0) $ map (flip mod) ms

p2 :: Problem
p2 input = (show.sum) $ filter even fib
    where
    size = read input :: Int
    fib = unfoldr uf (0, 1)
    uf (a, b)
        | s < size = Just (s, (b, s))
        | otherwise = Nothing
        where s = a + b

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
p14 input = show.snd.maximum $ map (\x -> ((length.collatz) x, (head.collatz) x)) [1..size]
    where
    size = read input - 1 :: Int
    collatz n = takeWhile (> 1) (iterate next n) ++ [1]
    next x
        | even x  = x `div` 2
        | odd  x  = 3 * x + 1
        | otherwise = undefined

p16 :: Problem
p16 input = show.sum $ map digitToInt $ show power
    where
    power = 2 ^ exp' :: Integer
    exp' = read input :: Integer

p17 :: Problem
p17 input = (show.length.concat) $ concatMap convert [1..size]
    where
    size = read input - 1 :: Int
    toTwenty = Map.fromList [(1, "one"),
        (2, "two"),
        (3, "three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"),
        (8, "eight"),
        (9, "nine"),
        (10, "ten"),
        (11, "eleven"),
        (12, "twelve"),
        (13, "thirteen"),
        (14, "fourteen"),
        (15, "fifteen"),
        (16, "sixteen"),
        (17, "seventeen"),
        (18, "eighteen"),
        (19, "nineteen")]
    toHundred = Map.fromList [(2, "twenty"),
        (3, "thirty"),
        (4, "forty"),
        (5, "fifty"),
        (6, "sixty"),
        (7, "seventy"),
        (8, "eighty"),
        (9, "ninety")]
    convert = unfoldr decompose
    decompose n
        | n == 0                        = Nothing
        | n < 20                        = (,) <$> stitch [Map.lookup n toTwenty] <*> pure 0
        | n >= 20 && n < 100            = (,) <$> stitch [Map.lookup (n `div` 10) toHundred] <*> pure ( n `rem` 10)
        | n < 1000 && n `mod` 100 == 0  = (,) <$> stitch [Map.lookup (n `div` 100) toTwenty, Just "hundred"] <*> pure 0
        | n > 100 && n <= 999           = (,) <$> stitch [Map.lookup (n `div` 100) toTwenty, Just "hundredand"] <*> pure ( n `rem` 100)
        | n == 1000                     = Just ("onethousand", 0)
        | otherwise                     = Nothing
    stitch l = concat <$> sequence l

p18 :: Problem
p18 input = (show.head) $ foldl foldtr (reduce (head triangle)) (tail triangle)
    where
    foldtr red row = reduce $ zipWith (+) red row
    triangle = reverse $ map (map read . splitOn " ") $ lines input :: [[Int]]
    reduce = unfoldr trans
    trans (x:y:ys) = Just (max x y, y:ys)
    trans [x] = Just (x, [])
    trans [] = Nothing

p19 :: Problem
p19 input = (show.length) $ filter (==(7, 1)) wd_pairs
    where
    years = read input :: Int
    weekdays = cycle [1..7]
    wd_pairs = zip weekdays (calendar years) :: [(Int, Int)]
    mult n a = concat $ replicate n a
    [january, march, may, july, august, october, december] = replicate 7 [1..31]
    [april, june, november, september] = replicate 4 [1..30]
    february = [1..28]
    leap_february = [1..29]
    rest_months = march ++ april ++ may ++ june ++ july ++ august ++ september ++ october ++ november ++ december
    year = january ++ february ++ rest_months
    leap_year = january ++ leap_february ++ rest_months
    calendar y = mult lc leap_year ++ mult (y - 1 - lc) year
        where
        lc = (y - 1) `div` 4

p20 :: Problem
p20 input = show.sum $ map digitToInt $ (show.product) [1..size]
    where
    size = read input :: Integer

p21 :: Problem
p21 input = show.sum $ map fst $ filter (uncurry (==))
    [(a, sumd b) | (a, b) <- [(a, b) | a <- [1..size - 1], let b = sumd a, a /= b]]
    where
    size = read input :: Integer
    sumd x = sum $ divisors x

p22 :: Problem
p22 input = show.sum $ zipWith (*) [1 ..] (map score $ sort names)
    where
    names = map strip $ splitOn "," $ concat $ lines input
    strip = dropWhileEnd (=='"') . dropWhile (=='"')
    score name = sum $ map (\x -> ord x - ord '@') name
    
p30 :: Problem
p30 input = show.sum $ filter (\n -> n == digPow n) [2..limit]
    where
    limit =   9^ (fst . head) (dropWhile (uncurry (<)) [(x, numDigits x) | x <- [1..]])
    powers = read input :: Integer
    digPow = sum . map ((^powers) . toInteger . digitToInt) . show
    numDigits n = length $ show (9^powers*n)

problems :: Problems
problems = [
    ("Problem 1", p1, return"1000"),
    ("Problem 2", p2, return"4000000"),
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
    ("Problem 14", p14, return "1000000"),
    ("Problem 15", p15, return "20"),
    ("Problem 16", p16, return "1000"),
    ("Problem 17", p17, return "1000"),
    ("Problem 18", p18, readFile "inputs/p18.txt"),
    ("Problem 19", p19, return "100"),
    ("Problem 20", p20, return "100"),
    ("Problem 21", p21, return "10000"),
    ("Problem 22", p22, readFile "inputs/p22.txt"),
    ("Problem 30", p30, return "5"),
    ("Problem 67", p18, readFile "inputs/p67.txt")]

main :: IO ()
main = mapM_ printProblem problems
