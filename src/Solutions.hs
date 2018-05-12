module Main where

import Numeric
import Data.Char
import Data.List
import Data.Ratio
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Control.Exception as Ex
import Safe

import System.Environment

import Euler.P15
import Euler.Util

p1 :: Solution
p1 input = (show.sum) $ filter (multOf [3, 5]) [1..size - 1]
    where
    size = read input :: Int
    multOf ms x = any (\f -> f x == 0) $ map (flip mod) ms

p2 :: Solution
p2 input = (show.sum) $ filter even $ takeWhile (< size) fibs
    where
    size = read input :: Integer

p3 :: Solution
p3 input = show $ maximum $ pfactors (read input)

p4 :: Solution
p4 input = show $ maximum $ filter isPalindrome numbers
    where
    isPalindrome x = let s = show x
                     in
                     s == reverse s
    numbers = [x*y | x <- [beg..end], y <- [x..end]] :: [Int]
    size = read input :: Int
    beg = 10 ^ (size - 1)
    end = (10 ^ size) - 1

p5 :: Solution
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

p6 :: Solution
p6 input = show $ (s * s) - sum [x*x | x <- [1..size]]
    where
    size = read input :: Int
    s = sum [1..100]

p7 :: Solution
p7 input = show $ primes !! upper
    where
    upper = read input - 1 :: Int

p8 :: Solution
p8 input = show $ maximum $ unfoldr grow digits
    where
    digits = map digitToInt $ (concat . lines) input
    grow seed = case splitAt 13 seed of
                     (_, [])  -> Nothing
                     (xs, _)  -> Just (product xs, drop 1 seed)

p9 :: Solution
p9 input = show $ reduce [[a, b, c] | a <- [1..sum'],
                                      b <- [1..sum' - a],
                                      let c = sum' - a - b,
                                      a*a + b*b == c*c]
    where
    reduce = product.head
    sum' = read input :: Int
    

p10 :: Solution
p10 input = show $ sum (takeWhile (< size) primes)
    where
    size = read input :: Int

p11 :: Solution
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

p12 :: Solution
p12 input = show $ snd . head $ dropWhile ((< size) . fst) (map cp triangle)
    where
    size = read input :: Int
    cp x = (length $ divisors x, x)
    triangle :: [Int]
    triangle = [sum [1..x] | x <- [1..]]

p13 :: Solution
p13 input = let numbers = map read $ lines input :: [Integer]
            in take 10 $ show.sum $ numbers

p14 :: Solution
p14 input = show.snd.maximum $ map (\x -> ((length.collatz) x, (head.collatz) x)) [1..size]
    where
    size = read input - 1 :: Int
    collatz n = takeWhile (> 1) (iterate next n) ++ [1]
    next x
        | even x  = x `div` 2
        | odd  x  = 3 * x + 1
        | otherwise = undefined

p16 :: Solution
p16 input = show.sum $ map digitToInt $ show power
    where
    power = 2 ^ exp' :: Integer
    exp' = read input :: Integer

p17 :: Solution
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

p18 :: Solution
p18 input = (show.head) $ foldl foldtr (reduce (head triangle)) (tail triangle)
    where
    foldtr red row = reduce $ zipWith (+) red row
    triangle = reverse $ map (map read . splitOn " ") $ lines input :: [[Int]]
    reduce = unfoldr trans
    trans (x:y:ys) = Just (max x y, y:ys)
    trans [x] = Just (x, [])
    trans [] = Nothing

p19 :: Solution
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

p20 :: Solution
p20 input = show.sum $ map digitToInt $ (show.product) [1..size]
    where
    size = read input :: Integer

p21 :: Solution
p21 input = show.sum $ map fst $ filter (uncurry (==))
    [(a, sumd b) | (a, b) <- [(a, b) | a <- [1..size - 1], let b = sumd a, a /= b]]
    where
    size = read input :: Integer
    sumd x = sum $ divisors x

p22 :: Solution
p22 input = show.sum $ zipWith (*) [1 ..] (map score $ sort names)
    where
    names = map strip $ splitOn "," $ concat $ lines input
    strip = dropWhileEnd (=='"') . dropWhile (=='"')
    score name = sum $ map (\x -> ord x - ord '@') name

p23 :: Solution
p23 input = show.sum $ [x | x <- [1..size], not $ Set.member x isAbundantSum]
    where
    size = read input :: Int
    sumd x = sum $ divisors x
    abundant = [x | x <- [1..size], x < sumd x]
    isAbundant = Set.fromList abundant
    isAbundantSum = Set.fromList [x + y | x <- abundant, y <- [x..size - x], Set.member y isAbundant]

p24 :: Solution
p24 input = concatMap show (last $ take size $ sort $ permutations digits)
    where
    digits = [0..9] :: [Int]
    size = read input :: Int

p25 :: Solution
p25 input = show $ (+1).length $ takeWhile (\a -> length (show a) < size) (1:fibs)
    where
    size = read input :: Int

p26 :: Solution
p26 input = show.snd.maximum $ zip recuring ([1..] :: [Int])
    where
    limit = read input :: Int
    recur n = unfoldr (\a -> Just (a `mod` n, 10 * (a `mod` n))) 10
    recuring = [length.Set.fromList $ take limit $ recur n | n <- [1..limit]] :: [Int]

p27 :: Solution
p27 input = show.product.snd.maximum $ [(plen a b, [a, b]) | a <- [-limit..limit], b <- [-limit..limit]]
    where
    plen a b = length $ takeWhile (\x -> (length . pfactors) x == 1) [q | n <- [0..], let q = n * n + a * n + b, q >= 0]
    limit = read input :: Int

p28 :: Solution
p28 input = show.sum.concat $ diagonals
    where
    size = read input :: Integer
    maxLevel = (size - 1) `div` 2
    diagonals = [1] : [take 4 $ iterate (\x -> x - 2 * l) s |
                       l <- [1..maxLevel], let a = 2 * l + 1,
                                           let s = a * a] :: [[Integer]]

p29 :: Solution
p29 input = show.length.nub $ [a ^ b | a <- [2..limit], b <- [2..limit]]
    where
    limit = read input :: Integer

p30 :: Solution
p30 input = show.sum $ filter (\n -> n == digPow n) [2..limit]
    where
    limit =   9^ (fst . head) (dropWhile (uncurry (<)) [(x, numDigits x) | x <- [1..]])
    powers = read input :: Integer
    digPow = sum . map ((^powers) . toInteger . digitToInt) . show
    numDigits n = length $ show (9^powers*n)

-- Solution to problem 31 emulates the imperative dynamic programming solution
-- and it is the first instance of solution that is a lot less elegant than
-- the original imperative version :(

p31 :: Solution
p31 input = show.last $ foldl (\a b -> foldl1 (zipWith (+)) (b a)) start repls
    where
    amount = read input :: Int
    repl n a = take (length a) [take (length a) $ replicate i 0 ++ a | i <- [0,n..]]
    repls = map repl [1, 2, 5, 10, 20, 50, 100, 200]
    start = 1 : replicate amount 0 :: [Integer]

p32 :: Solution
p32 input = show.sum.nub $ map (uncurry (*)) candidates
    where
    limit = read input :: Int
    candidates = filter (\(n,m) -> isPandigit $ toStr (n, m))
                 [(n, m) | n <- [1..limit - 1], m <- [1..limit `div` n]] :: [(Int, Int)]
    toStr (n, m) = show n ++ show m ++ show (m*n)
    isPandigit n = let m = filter (/= '0') n in
                           (length m == 9) && length m == length (Set.fromList m)

p33 :: Solution
p33 _ = show.denominator.product $ ([a % c | a <- [1..9],
                                             b <- [1..9],
                                             c <- [1..9],
                                             isCancelling a b c,
                                             a /= b && a/= c] :: [Ratio Integer])
    where isCancelling a b c = ((10 * a + b)%(10 * b + c)) == (a % c)


p34 :: Solution
p34 _ = show.sum $ [x | x <- [3..limit+1], fsum x]
    where
    fsum x = x == sum (map (factorial . digitToInt) $ show x)
    factorial n = factorials !! n
    factorials = 1 : map (\n -> product [1..n]) [1..9]
    limit = snd.head $ dropWhile (uncurry (<)) [(10^x, x * factorials !! 9) | x <- [1..]] :: Int

p35 :: Solution
p35 input = show.length $ filter iscircular $ takeWhile (<limit) primes
    where
    limit = read input :: Int
    iscircular n = all isprime $ rotations n
    rotations n = take l $ map (read . take l) $ iterate (drop 1) $ cycle s
        where
        s = show n
        l = length s

p36 :: Solution
p36 input = show.sum $ filter f [1..limit]
    where
    limit = read input :: Int
    ispalindrome s = s == reverse s
    tobin n = showIntAtBase 2 intToDigit n ""
    f x = ispalindrome (show x) && ispalindrome (tobin x)


p37 :: Solution
p37 _ = show.sum $ take 11 $ dropWhile (<8) $ filter isTrunc primes
    where
    isTruncLeft s = all isprime $ take l $ map read $ iterate (drop 1) s
        where
        l = length s
    isTruncRight s = all isprime $ take l $ map read $ iterate dropLast s
        where
        l = length s
        dropLast xs = take (length xs - 1) xs
    isTrunc n = isTruncLeft s && isTruncRight s
        where
        s = show n

solutions :: Map.Map Int (Solution, IO String)
solutions = Map.fromList [
   (  1, ( p1, return"1000")),
   (  2, ( p2, return"4000000")),
   (  3, ( p3, return"600851475143")),
   (  4, ( p4, return "3")),
   (  5, ( p5, return "20")),
   (  6, ( p6, return "100")),
   (  7, ( p7, return "10001")),
   (  8, ( p8, readFile "inputs/p8.txt")),
   (  9, ( p9, return "1000")),
   ( 10, (p10, return "2000000")),
   ( 11, (p11, readFile "inputs/p11.txt")),
   ( 12, (p12, return "500")),
   ( 13, (p13, readFile "inputs/p13.txt")),
   ( 14, (p14, return "1000000")),
   ( 15, (p15, return "20")),
   ( 16, (p16, return "1000")),
   ( 17, (p17, return "1000")),
   ( 18, (p18, readFile "inputs/p18.txt")),
   ( 19, (p19, return "100")),
   ( 20, (p20, return "100")),
   ( 21, (p21, return "10000")),
   ( 22, (p22, readFile "inputs/p22.txt")),
   ( 23, (p23, return "28123")),
   ( 24, (p24, return "1000000")),
   ( 25, (p25, return "1000")),
   ( 26, (p26, return "1000")),
   ( 27, (p27, return "1000")),
   ( 28, (p28, return "1001")),
   ( 29, (p29, return "100")),
   ( 30, (p30, return "5")),
   ( 31, (p31, return "200")),
   ( 32, (p32, return "10000")),
   ( 33, (p33, return "")),
   ( 34, (p34, return "")),
   ( 35, (p35, return "1000000")),
   ( 36, (p36, return "1000000")),
   ( 37, (p37, return "")),
   ( 67, (p18, readFile "inputs/p67.txt"))]

mayFile :: FilePath -> MaybeT IO String
mayFile fp = do
    res <- liftIO $ Ex.try (readFile fp) :: MaybeT IO (Either Ex.SomeException String)
    case res of
         Right contents -> return contents
         Left _ -> MaybeT $ return Nothing

getInput :: String -> IO String
getInput i = do
    res <- runMaybeT $ MaybeT (return number) <|> (pure i >>= mayFile) <|> pure i
    case res of
         Nothing -> undefined -- will never happen, as the last alternative is 'pure i'
         Just s  -> return s
    where
    number = show <$> (readMay i :: Maybe Int)
    
handle :: [String] -> IO ()
handle (a1:a2:_) = case Map.lookup p solutions of
                        Nothing -> putStrLn "Problem not yet solved !!!"
                        Just (pr, _) -> printSolution (p, pr, getInput a2)
                        where
                        p = read a1 :: Int

handle (a:_) = case Map.lookup p solutions of
                    Nothing -> putStrLn "Problem not yet solved !!!"
                    Just (pr, i) -> printSolution (p, pr, i)
                    where
                    p = read a :: Int

handle [] = mapM_ printSolution solutions'
    where
    solutions' = map (\(n, (p, i)) -> (n, p, i)) $ Map.toList solutions

main :: IO ()
main = do
    args <- getArgs
    handle args
