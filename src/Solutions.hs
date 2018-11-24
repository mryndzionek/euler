module Main where

import Numeric
import Data.Char
import Data.List
import Data.Array (Array, (!), array, assocs)
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Tuple
import Data.Bits
import qualified Data.Permute as Per
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Numbers.Primes

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Control.Exception as Ex
import Safe

import System.Environment

import Euler.P15
import Euler.P51
import Euler.P54
import Euler.P60
import Euler.P61
import Euler.P62
import Euler.P64_66
import Euler.P68
import Euler.P83
import Euler.P84
import Euler.Util

p1 :: Int -> Int
p1 size = sum $ filter (multOf [3, 5]) [1..size - 1]
    where
    multOf ms x = any (\f -> f x == 0) $ map (flip mod) ms

p2 :: Integer -> Integer
p2 size = sum $ filter even $ takeWhile (< size) fibs

p3 :: Int -> Int
p3 input = maximum $ pfactors input

p4 :: Int -> Int
p4 size = maximum $ filter isPalindrome numbers
    where
    isPalindrome x = let s = show x
                     in
                     s == reverse s
    numbers = [x*y | x <- [beg..end], y <- [x..end]] :: [Int]
    beg = 10 ^ (size - 1)
    end = (10 ^ size) - 1

p5 :: Int -> Int
p5 size = reduce . lp . nub . concat $ map (count . pfactors) [2..size]
    where
    reduce :: [(Int, Int)] -> Int
    reduce = product . map (uncurry (^))
    lp a = Map.toList $ foldr f Map.empty a
        where
        f (x, y) m = case Map.lookup x m of
                     Nothing -> Map.insert x y m
                     Just _  -> Map.adjust (max y) x m

p6 :: Int -> Int
p6 size = (s * s) - sum [x*x | x <- [1..size]]
    where
    s = sum [1..100]

p7 :: Int -> Integer
p7 a = primes !! (a - 1) :: Integer

p8 :: Str -> Int
p8 (Str input) = maximum $ unfoldr grow digits
    where
    digits = map digitToInt $ (concat . lines) input
    grow seed = case splitAt 13 seed of
                     (_, [])  -> Nothing
                     (xs, _)  -> Just (product xs, drop 1 seed)

p9 :: Int -> Int
p9 sum' = reduce [[a, b, c] | a <- [1..sum'],
                               b <- [1..sum' - a],
                               let c = sum' - a - b,
                               a*a + b*b == c*c]
    where
    reduce = product.head
    
p10 :: Int -> Int
p10 size = sum (takeWhile (< size) primes)

p11 :: Str -> (Maybe Int, Maybe [Int])
p11 (Str input) = maximum $ map (\x -> (product <$> x, x)) g
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

p12 :: Int -> Int
p12 size = snd . head $ dropWhile ((< size) . fst) (map cp triangle)
    where
    cp x = (length $ divisors x, x)
    triangle :: [Int]
    triangle = [sum [1..x] | x <- [1..]]

p13 :: Str -> String
p13 (Str input) = let numbers = map read $ lines input :: [Integer]
            in take 10 $ show.sum $ numbers

p14 :: Int -> Int
p14 size = snd.maximum $ map ((length . collatz) &&& (head . collatz)) [1..size - 1]
    where
    collatz n = takeWhile (> 1) (iterate next n) ++ [1]
    next x
        | even x  = x `quot` 2
        | odd  x  = 3 * x + 1
        | otherwise = undefined

p16 :: Integer -> Int
p16 exp' = sum . map digitToInt $ show power
    where
    power = 2 ^ exp' :: Integer

p17 :: Int -> Int
p17 size = length . concat $ concatMap convert [1..size]
    where
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
        | n >= 20 && n < 100            = (,) <$> stitch [Map.lookup (n `quot` 10) toHundred] <*> pure ( n `rem` 10)
        | n < 1000 && n `rem` 100 == 0  = (,) <$> stitch [Map.lookup (n `quot` 100) toTwenty, Just "hundred"] <*> pure 0
        | n > 100 && n <= 999           = (,) <$> stitch [Map.lookup (n `quot` 100) toTwenty, Just "hundredand"] <*> pure ( n `rem` 100)
        | n == 1000                     = Just ("onethousand", 0)
        | otherwise                     = Nothing
    stitch l = concat <$> sequence l

p18 :: Str -> Int
p18 (Str input) = head $ foldl foldtr (reduce (head triangle)) (tail triangle)
    where
    foldtr red row = reduce $ zipWith (+) red row
    triangle = reverse $ map (map read . splitOn " ") $ lines input :: [[Int]]
    reduce = unfoldr trans
    trans (x:y:ys) = Just (max x y, y:ys)
    trans [x] = Just (x, [])
    trans [] = Nothing

p19 :: Int -> Int
p19 years = length $ filter (==(7, 1)) wd_pairs
    where
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
        lc = (y - 1) `quot` 4

p20 :: Integer -> Int
p20 size = sum $ map digitToInt $ (show.product) [1..size]

p21 :: Integer -> Integer
p21 size = sum $ map fst $ filter (uncurry (==))
    [(a, sumd b) | (a, b) <- [(a, b) | a <- [1..size - 1], let b = sumd a, a /= b]]
    where
    sumd x = sum $ divisors x

p22 :: Str -> Int
p22 (Str input) = sum $ zipWith (*) [1 ..] (map score $ sort names)
    where
    names = map strip $ splitOn "," $ concat $ lines input
    strip = dropWhileEnd (=='"') . dropWhile (=='"')
    score name = sum $ map (\x -> ord x - ord '@') name

p23 :: Int -> Int
p23 size = sum [x | x <- [1..size], not $ Set.member x isAbundantSum]
    where
    sumd x = sum $ divisors x
    abundant = [x | x <- [1..size], x < sumd x]
    isAbundant = Set.fromList abundant
    isAbundantSum = Set.fromList [x + y | x <- abundant, y <- [x..size - x], Set.member y isAbundant]

p24 :: Int -> [Int]
p24 size = Per.elems . last $ take size perm
    where
    perm = perm' $ Just $ Per.permute 10
    perm' p = case p of
                   Just p' -> p' : perm' (Per.next p')
                   Nothing -> []

p25 :: Int -> Int
p25 size = (+1).length $ takeWhile (\a -> length (show a) < size) (1:fibs)

p26 :: Int -> Int
p26 limit = snd . maximum $ zip recuring ([1..] :: [Int])
    where
    recur n = unfoldr (\a -> Just (a `rem` n, 10 * (a `rem` n))) 10
    recuring = [length.Set.fromList $ take limit $ recur n | n <- [1..limit]] :: [Int]

p27 :: Int -> Int
p27 limit = product.snd.maximum $ [(plen a b, [a, b]) | a <- [-limit..limit], b <- [-limit..limit]]
    where
    plen a b = length $ takeWhile isPrime [q | n <- [0..], let q = n * n + a * n + b, q >= 0]

p28 :: Integer -> Integer
p28 size = sum.concat $ diagonals
    where
    maxLevel = (size - 1) `quot` 2
    diagonals = [1] : [take 4 $ iterate (\x -> x - 2 * l) s |
                       l <- [1..maxLevel], let a = 2 * l + 1,
                                           let s = a * a] :: [[Integer]]

p29 :: Integer -> Int
p29 limit = length.nub $ [a ^ b | a <- [2..limit], b <- [2..limit]]

p30 :: Integer -> Integer
p30 powers = sum $ filter (\n -> n == digPow n) [2..limit]
    where
    limit =   9^ (fst . head) (dropWhile (uncurry (<)) [(x, numDigits x) | x <- [1..]])
    digPow = sum . map ((^powers) . toInteger . digitToInt) . show
    numDigits n = length $ show (9^powers*n)

p31 :: [Int] -> Int -> Integer
p31 coins amount = ways amount coins
    where
    ways :: Int -> [Int] -> Integer
    ways n c = last . snd $ until (null . fst) way (c, 1 : replicate n 0)
    way (c, s) = (drop 1 c, zipWith (+) s (replicate (head c) 0 ++ snd (way (c, s))))

p32 :: Int -> Int
p32 limit = sum.nub $ map (uncurry (*)) candidates
    where
    candidates = filter (\(n,m) -> isPandigit $ toStr (n, m))
                 [(n, m) | n <- [1..limit - 1], m <- [1..limit `quot` n]] :: [(Int, Int)]
    toStr (n, m) = show n ++ show m ++ show (m*n)
    isPandigit n = let m = filter (/= '0') n in
                           (length m == 9) && length m == length (Set.fromList m)

p33 :: () -> Integer
p33 _ = denominator.product $ ([a % c | a <- [1..9],
                                             b <- [1..9],
                                             c <- [1..9],
                                             isCancelling a b c,
                                             a /= b && a/= c] :: [Ratio Integer])
    where isCancelling a b c = ((10 * a + b)%(10 * b + c)) == (a % c)

p34 :: () -> Int
p34 _ = sum [x | x <- [3..limit+1], fsum x]
    where
    fsum x = x == sum (map (factorial . digitToInt) $ show x)
    factorial n = factorials !! n
    factorials = 1 : map (\n -> product [1..n]) [1..9]
    limit = snd.head $ dropWhile (uncurry (<)) [(10^x, x * factorials !! 9) | x <- [1..]] :: Int

p35 :: Int -> Int
p35 limit = length $ filter iscircular $ takeWhile (<limit) primes
    where
    iscircular n = all isPrime $ rotations n
    rotations :: Int -> [Int]
    rotations n = take l $ map (read . take l) $ iterate (drop 1) $ cycle s
        where
        s = show n
        l = length s

p36 :: Int -> Int
p36 limit = sum $ filter f [1..limit]
    where
    ispalindrome s = s == reverse s
    tobin n = showIntAtBase 2 intToDigit n ""
    f x = ispalindrome (show x) && ispalindrome (tobin x)

p37 :: () -> Integer
p37 _ = sum $ take 11 $ dropWhile (<8) (filter isTrunc primes :: [Integer])
    where
    isTruncLeft s = all (isPrime :: Integer -> Bool) $ take l $ map read $ iterate (drop 1) s
        where
        l = length s
    isTruncRight s = all (isPrime :: Integer -> Bool) $ take l $ map read $ iterate dropLast s
        where
        l = length s
        dropLast xs = take (length xs - 1) xs
    isTrunc n = isTruncLeft s && isTruncRight s
        where
        s = show n

p38 :: () -> String
p38 _ = maximum $ filter isPandigit candidates
    where
    candidates = concatMap candidate ranges
    candidate a = takeWhile ((<10).length) $ scanl1 (++) $ map (show . (a *)) [1 ..]
    ranges = concat [[99, 98..91], [999, 998..918], [9999, 9998..9182]] :: [Integer]
    isPandigit n = let m = filter (/= '0') n in
                           (length m == 9) && length m == length (Set.fromList m)

p39 :: Int -> Int
p39 limit = (snd . maximum) [(length $ triplets p, p) | p <- [2..limit]]
    where
    triplets p = [(a, b, c) | a <- [2..p `quot` 3],
                            let b = p * (p - 2 * a) `quot` (2 * (p - a)),
                            let c = p - a - b,
                            b > a,
                            p * (p - 2 * a) `rem` (2 * (p - a)) == 0]

p40 :: Int -> Int
p40 limit = product [d (10^a) | a <- [0..limit]]
    where
    d n = digitToInt.last $ take n (concatMap show ([1..] :: [Int]))

p41 :: () -> Integer
p41 _ = maximum $ filter isPrime $ concatMap pandigits [4, 7]
    where
    pandigits :: Int -> [Integer]
    pandigits n = map read $ permutations [intToDigit a | a <- [1..n]]

p42 :: Str -> Int
p42 (Str input) = length $ filter (`Set.member` triangles) scores
    where
    scores = map wordToNum names
    triangles = Set.fromList $ takeWhile (<=maximum scores) [n * (n + 1) `quot` 2 | n <- [1..]]
    wordToNum w = sum $ map (\x -> ord x - 64) w
    strip = dropWhileEnd (=='"') . dropWhile (=='"')
    names = map strip $ splitOn "," $ concat $ lines input

p43 :: () -> Int
p43 _ = sum (map (read . map intToDigit) found :: [Int])
    where
    found = filter test [p | p <- permutations [0..9], p !! 5 == 5]
    test p = all ((==0).uncurry mod) $ zip (subs p) (primes :: [Integer])
    subs p = map (read . (map intToDigit . (\ x -> (\ y z -> take 3 $ drop y z) x p))) [1 .. 7]

p44 :: () -> Int
p44 _ = head candidates
    where
    pentagonals = [(n*(3*n - 1)) `quot` 2 | n <- [1..]]
    isPentagonal n = v == fromInteger (round v) && (round v `rem` 6 :: Int) == 0
        where
        v = isqrt (1 + 24 * n) + 1
        isqrt = (sqrt . fromIntegral) :: Int -> Double
    candidates = [j - k | j <- pentagonals, k <- takeWhile (< j) pentagonals,
                      isPentagonal (j - k), isPentagonal (j + k)]

p45 :: Int -> Int
p45 start = head [t | t <- triangles, isPentagonal t, isHexagonal t]
    where
    triangles = [n * (n + 1) `quot` 2 | n <- [start..]]
    isqrt = (sqrt . fromIntegral) :: Int -> Double
    isPentagonal n = v == fromInteger (round v) && (round v `rem` 6 :: Int) == 0
        where
        v = isqrt (1 + 24 * n) + 1
    isHexagonal n = v == fromInteger (round v) && (round v `rem` 4 :: Int) == 0
        where
        v = isqrt (1 + 8 * n) + 1

p46 :: () -> Integer
p46 _ = head $ dropWhile check ([9,11..] :: [Integer])
    where
    check n = any isTwice $ map (n-) $ takeWhile (<= n) primes
    isTwice m = v == fromInteger (round v)
        where
        v = sqrt (fromIntegral m / 2) :: Double

p47 :: Int -> Int
p47 len = get' $ filter check [map facts [a..a + len - 1] | a <- [0..]]
    where
    get' = product.head.head
    check p = all ((== len) . length) p && (m == nub m)
        where
            m = concat p
    facts n = map (uncurry (*)) $ (count.pfactors) n

p48 :: Integer -> Integer
p48 limit = last10digits $ sum [n ^ n | n <- [1..limit]]
    where
    last10digits n = n `rem` (10^(10 :: Integer))

p49 :: () -> Integer
p49 _ = flip (!!) 1 $ do
    candidate <- takeWhile (<10000) $ dropWhile (<999) (primes :: [Integer])
    step <- [1..9999]
    let candidates = [candidate, candidate + step, candidate + 2 * step]
    let sets = map (Set.fromList.show) candidates
    guard $ candidate + 2 * step < 10000
    guard $ all isPrime candidates
    guard $ all (== head sets) sets
    return . read $ concatMap show candidates

p50 :: Int -> (Int, Int)
p50 limit = maximum $ filter (isPrime.snd) $ concatMap gen [take n sums | n <- [1..length sums]]
    where
    gen n = zip [length n, length n - 1..] (map (last n -) (0:n))
    sums = takeWhile (<limit) cumul
    cumul = scanl1 (+) primes

p52 :: Int -> Int
p52 upTo = head $ do
    n <- [1..] :: [Int]
    let candidates = map (show.(*) n) [1..upTo]
    guard $ all (== Set.fromList (head candidates)) $ map Set.fromList (tail candidates)
    return n

p53 :: Integer -> Int
p53 limit = length $ filter (>limit) $ map combi perms
    where
    factorials = 1 : map (\n -> product [1..n]) [1..]
    combi (r, n) = factorials !! n `quot` (factorials !! r * factorials !! (n - r))
    perms = [(n, x) | x <- [1..100], n <- [1..x]]

p55 :: Integer -> Int
p55 limit = length $ filter (not . any isPalindr . candidates) [1..limit]
    where
    rev = read . reverse . show
    isPalindr n = n == rev n
    candidates n = take 50 . drop 1 $ iterate (\x -> x + rev x) n

p56 :: Integer -> Int
p56 limit = maximum $ map digitSum [a ^ b | a <- [1..limit], b <- [1..limit]]
    where
    digitSum = sum . map digitToInt . show

p57 :: Int -> Int
p57 limit = length $ filter isLonger $ take limit expansion
    where
    isLonger :: Ratio Integer -> Bool
    isLonger n = length (show $ numerator n) > length (show $ denominator n)
    expansion = iterate (\x -> 1 + 1 / (1 + x)) 1

p58 :: Int -> Integer
p58 input = fst.head $ dropWhile ((>ratio).snd) $ zip ([3,5..] :: [Integer]) $ zipWith (%) prime (5 : [9,13..])
    where
    ratio = input % 100 :: Ratio Int
    diagonals = [take 4 $ iterate (\x -> x - 2 * l) s |
                       l <- [1..], let a = 2 * l + 1,
                                   let s = a * a] :: [[Integer]]
    prime = drop 1 $ scanl (\x y -> x + length (filter isPrime y)) 0 diagonals

p59 :: Str -> Int
p59 (Str input) = sum $ zipWith xor ciphertxt (cycle key)
    where
    ciphertxt = map read $ splitOn "," input
    chunks = transpose $ chunksOf 3 ciphertxt
    maxCount :: [Int] -> (Integer, Int)
    maxCount x = maximum $ map swap $ count x
    key = map ((xor (ord ' ') . snd) . maxCount) chunks

p63 :: () -> Integer
p63 _ = sum ([floor(1 / ((1 :: Double) - logBase 10 n)) | n <- [1..9]] :: [Integer])

p69 :: Integer -> Integer
p69 limit = last $ takeWhile (<limit) $ map product [take n primes | n <- [1..]]

p70 :: Integer -> Integer
p70 limit = snd $ minimum ([(divi n phi, n) | (n, phi) <- candidates, perm n phi] :: [(Double, Integer)])
    where
        divi a b = fromIntegral a / fromIntegral b
        perm x y = sort (show x) == sort (show y)
        totient p_1 p_2 = (p_1 - 1) * (p_2 - 1)
        candidates = [(n, phi) | p_1 <- ps, p_2 <- dropWhile (<= p_1) ps,
                                 let n = p_1 * p_2, n <= limit,
                                 let phi = totient p_1 p_2]
            where ps = takeWhile (<=2 * pl) primes
                  pl = head $ dropWhile ((<=limit) . (^(2 :: Integer))) primes

p71 :: Integer -> Ratio Integer
p71 limit = fst $ until (\(x, y) -> denominator (mediant x y) > limit) farey (0, 1)
    where
    farey (a, b) = let m = mediant a b in if m < (3 % 7) then (m, b) else (a, m)
    mediant a b = (numerator a + numerator b) % (denominator a + denominator b)

p72 :: Int -> Int
p72 limit = fareySum limit - 2
    where
    fareySum m = map f [0..] !! m
        where
        f n = (n*(n + 3)) `quot` 2 - sum [fareySum (n `quot` k) | k <- [2..n]]

p73 :: Int -> Int
p73 limit = length $ farey limit
    where
    farey l = drop 1 $ iFarey (1 % 3, 1 % 2)
        where
        iFarey (a, b)
            | denominator mediant > l = [a]
            | otherwise = iFarey (a, mediant) ++ iFarey (mediant, b)
            where
            mediant = (numerator a + numerator b) % (denominator a + denominator b)

p74 :: Int -> Int
p74 limit = length $ filter (==60) $ map (length . chain) [1..limit]
    where
    facSum n = sum $ map ((factorials !!) . digitToInt) $ show n
    factorial m = product [1..m]
    factorials = map factorial [0..9]
    chain :: Int -> [Int]
    chain n = evalState (build n) []
        where
        build :: Int -> State [Int] [Int]
        build m = do
            s <- get
            if m `elem` s then return s else
                modify (\ s' -> m : s') >> build (facSum m)

p75 :: Integer -> Int       
p75 limit = length $ filter ((==(1 :: Integer)) . snd) $ count $ do
    let mlimit = floor $ sqrt ((fromIntegral limit :: Double) / 2) :: Integer
    m <- [2..mlimit]
    n <- [1..m]
    guard $ (m + n) `rem` 2 == 1
    guard $ gcd n m == 1
    let triplet = [(m ^ (2 :: Integer)) - (n ^ (2 :: Integer)),
                   2 * m * n,
                   (m ^ (2 :: Integer)) + (n ^ (2 :: Integer))]
    let s = sum triplet
    [s * i | i <- [1..limit `quot` s]]

p77 :: Integer -> (Int, Integer)
p77 limit = head $ dropWhile ((< limit) . snd) candidates
    where
    candidates = [(last p, p31 p (last p)) | n <- [1..], let p = take n primes]

p78 :: Integer -> Integer
p78 limit = head [i | (i, p) <- assocs lut, p `rem` limit == 0]
    where
    pentagonals = sort [n * (3 * n - 1) `quot` 2 | n <- [-250..250], n /= 0]
    terms m = zip (cycle [1, 1, -1, -1]) (takeWhile (<= m) pentagonals)
    lut :: Array Integer Integer
    lut = array (0, limit) [(x, part x) | x <- [0..limit]]
    part n | n <  0 = 0
           | n == 0 = 1
           | otherwise = sum [s * (lut ! (n - p)) | (s, p) <- terms n]

p79 :: Str -> String
p79 (Str keylog) = concatMap (show . snd) $ sort' $ map (\(a, b) -> ((length . nub) b, a)) prec
    where
    attempts = map (map digitToInt ) $ lines keylog
    prec  = Map.toList $ Map.fromListWith (++) $ concatMap assoc attempts
    sort' = sortBy (flip compare)
    assoc = unfoldr f
        where   
        f [] = Nothing
        f (x:xs) = Just ((x, xs), xs)

p80 :: Integer -> Int
p80 limit = sum $ concatMap (sqrtDigits 99) [n | n <- [1..limit], not $ isSquare n]
    where
    isSquare :: Integer -> Bool
    isSquare n = let s = ((floor :: Double -> Integer).sqrt.fromIntegral) n in (s * s) == n
    isqrt 0 = 0
    isqrt 1 = 1
    isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)
    digits = map digitToInt . show :: Integer -> [Int]
    sqrtDigits :: Int -> Integer -> [Int]
    sqrtDigits c x = digits $ isqrt $ x * (10 ^ (2 * c))

p81 :: Str -> Integer
p81 (Str input) = head $ foldr fld (scanr1 (+) $ last matrix) (init matrix)
        where
        matrix = (map (map read . splitOn ",") . lines) input
        fld :: [Integer] -> [Integer] -> [Integer]
        fld cur prev = init $ scanr (\(p, c) a -> c + min a p) (last prev) (zip prev cur)

p82 :: Str -> Integer
p82 (Str input) = minimum $ foldr1 fld1 matrix
        where
        matrix = transpose $ (map (map read . splitOn ",") . lines) input
        fld1 :: [Integer] -> [Integer] -> [Integer]
        fld1 c p = let x = fld c p
                       y = fld (reverse c) (reverse p)
                    in zipWith min x (reverse y)
        fld :: [Integer] -> [Integer] -> [Integer]
        fld c' p' = tail $ scanl (\a (p, c) -> c + min a p) (head p') (zip p' c')

p85 :: Integer -> Integer
p85 limit = let asum x = x * (x + 1) `div` 2
            in snd $ minimum [(abs (asum x * asum y - limit), x * y) | x <- [1 .. 2000], y <- [1 .. 2000]]

p86 :: Integer -> Integer
p86 limit = fst . head $ dropWhile ((<limit) . snd) isum
        where
            candidates = [(l, wh) | l <- [3 .. ], wh <- [3 .. 2 * l],
                            let s = (sqrt . fromInteger) (wh * wh + l * l),
                            s == (fromInteger (round s) :: Double)]
            nsol l wh = if wh <= l then floor (halve wh)
                                   else 1 + l - ceiling (halve wh)
                where
                    halve :: Integer -> Double
                    halve a = fromIntegral a / 2
            isum = scanl' (\(_, s) (l, wh) -> (l, s + nsol l wh)) (0, 0) candidates

p87 :: Int -> Int
p87 limit = length . Set.fromList . filter (< limit) $ nums
    where
    plimit = floor (sqrt (fromIntegral limit) :: Double) :: Integer
    primes' = map fromIntegral $ takeWhile (<= plimit) primes
    f a b c = (a ^ (2 :: Int)) + (b ^ (3 :: Int)) + (c ^ (4 :: Int))
    nums = f <$> primes' <*> primes' <*> primes'

p88 :: Int -> Int
p88 limit = sum . nub . drop 2 . Map.elems $ Map.foldlWithKey f'' accum cache
        where
        cache = foldl' f Map.empty [2 .. 2 * limit]
        accum = Map.fromList [(n, 2 * n) | n <- [0 .. limit]]
        f m n = case Map.lookup n m of
                     Just _  -> m
                     Nothing -> foldl' (f' n) m [d | d <- [2 .. n], d * d <= n]
        f' n m d = let (q, r) = divMod n d
                       k = n - d - q + 2
                       is = Set.insert k . Set.map ((k - 1) +) $ fromMaybe Set.empty (Map.lookup q m)
                 in if r /= 0 then m
                    else case Map.lookup n m of
                              Just _  -> Map.adjust (`Set.union` is) n m
                              Nothing -> Map.insert n is m
        f'' m n k = let m1 = Map.filter (> n) $ Map.restrictKeys m k
                        m2 = Map.fromList $ zip (Map.keys m1) $ repeat n
                    in Map.union m2 m  

p89 :: Str -> Int
p89 (Str input) = sum $ map (sum . map gain . romCut) literals
    where
    literals = lines input
    lut = ["I", "IIII", "IV", "V", "VIIII", "IX", "X", "XXXX", "XL", "L",
           "LXXXX", "XC", "C", "CCCC", "CD", "D", "DCCCC", "CM", "M"]
    romCut = unfoldr (\l -> let p = last . filter (`isPrefixOf` l) $ lut
                            in if null l then Nothing else Just (p, drop (length p) l))
    gain :: String -> Int
    gain l
        | l `elem` ["VIIII", "DCCCC", "LXXXX"] = 3
        | l `elem` ["IIII", "XXXX", "CCCC"] = 2
        | otherwise = 0

p90 :: () -> Int
p90 _ = length $ filter (\x -> all (`elem` x) sq) ds
    where
    sq = [1, 4, 6, 16, 25, 36, 46, 64, 81]
    ds = map expand . Set.toList . Set.fromList $
         let dices = nub . map sort $ nPerms 6 [0..9] :: [[Int]]
         in (\d1 d2 -> if d1 < d2 then (d1, d2) else (d2, d1)) <$> dices <*> dices
    expand (da, db) = nub . concat $
        (\a b -> let a' = if a == 9 then 6 else a
                     b' = if b == 9 then 6 else b
                 in [a' * 10 + b', b' * 10 + a']) <$> da <*> db

solutions :: Map.Map Int (IO Solution)
solutions = Map.fromList [
   (  1, mkSol (p1, return 1000, 233168)),
   (  2, mkSol (p2, return 4000000, 4613732)),
   (  3, mkSol (p3, return 600851475143, 6857)),
   (  4, mkSol (p4, return 3, 906609)),
   (  5, mkSol (p5, return 20, 232792560)),
   (  6, mkSol (p6, return 100, 25164150)),
   (  7, mkSol (p7, return 10001, 104743)),
   (  8, mkSol (p8, Str <$> readFile "inputs/p8.txt", 23514624000)),
   (  9, mkSol (p9, return 1000, 31875000)),
   ( 10, mkSol (p10, return 2000000, 142913828922)),
   ( 11, mkSol (p11, Str <$> readFile "inputs/p11.txt", (Just 70600674,Just [89,94,97,87]))),
   ( 12, mkSol (p12, return 500, 76576500)),
   ( 13, mkSol (p13, Str <$> readFile "inputs/p13.txt", "5537376230")),
   ( 14, mkSol (p14, return 1000000, 837799)),
   ( 15, mkSol (p15, return 20, 137846528820)),
   ( 16, mkSol (p16, return 1000, 1366)),
   ( 17, mkSol (p17, return 1000, 21124)),
   ( 18, mkSol (p18, Str <$> readFile "inputs/p18.txt", 1074)),
   ( 19, mkSol (p19, return 100, 171)),
   ( 20, mkSol (p20, return 100, 648)),
   ( 21, mkSol (p21, return 10000, 31626)),
   ( 22, mkSol (p22, Str <$> readFile "inputs/p22.txt", 871198282)),
   ( 23, mkSol (p23, return 28123, 4179871)),
   ( 24, mkSol (p24, return 1000000, [2,7,8,3,9,1,5,4,6,0])),
   ( 25, mkSol (p25, return 1000, 4782)),
   ( 26, mkSol (p26, return 1000, 983)),
   ( 27, mkSol (p27, return 1000, -59231)),
   ( 28, mkSol (p28, return 1001, 669171001)),
   ( 29, mkSol (p29, return 100, 9183)),
   ( 30, mkSol (p30, return 5, 443839)),
   ( 31, mkSol (p31 [1, 2, 5, 10, 20, 50, 100, 200], return 200, 73682)),
   ( 32, mkSol (p32, return 10000, 45228)),
   ( 33, mkSol (p33, return (), 100)),
   ( 34, mkSol (p34, return (), 40730)),
   ( 35, mkSol (p35, return 1000000, 55)),
   ( 36, mkSol (p36, return 1000000, 872187)),
   ( 37, mkSol (p37, return (), 748317)),
   ( 38, mkSol (p38, return (), "932718654")),
   ( 39, mkSol (p39, return 1000, 840)),
   ( 40, mkSol (p40, return 6, 210)),
   ( 41, mkSol (p41, return (), 7652413)),
   ( 42, mkSol (p42, Str <$> readFile "inputs/p42.txt", 162)),
   ( 43, mkSol (p43, return (), 16695334890)),
   ( 44, mkSol (p44, return (), 5482660)),
   ( 45, mkSol (p45, return 286, 1533776805)),
   ( 46, mkSol (p46, return (), 5777)),
   ( 47, mkSol (p47, return 4, 134043)),
   ( 48, mkSol (p48, return 1000, 9110846700)),
   ( 49, mkSol (p49, return (), 296962999629)),
   ( 50, mkSol (p50, return 1000000, (543,997651))),
   ( 51, mkSol (p51, return 6, "*2*3*3")),
   ( 52, mkSol (p52, return 6, 142857)),
   ( 53, mkSol (p53, return 1000000, 4075)),
   ( 54, mkSol (p54, Str <$> readFile "inputs/p54.txt", Just 376)),
   ( 55, mkSol (p55, return 9999, 249)),
   ( 56, mkSol (p56, return 99, 972)),
   ( 57, mkSol (p57, return 1000, 153)),
   ( 58, mkSol (p58, return 10, 26241)),
   ( 59, mkSol (p59, Str <$> readFile "inputs/p59.txt", 107359)),
   ( 60, mkSol (p60, return 10000, 26033)),
   ( 61, mkSol (p61, return 9999, 28684)),
   ( 62, mkSol (p62, return (), 127035954683)),
   ( 63, mkSol (p63, return (), 49)),
   ( 64, mkSol (p64, return 10000, 1322)),
   ( 65, mkSol (p65, return 100, 272)),
   ( 66, mkSol (p66, return 1000, 661)),
   ( 67, mkSol (p18, Str <$> readFile "inputs/p67.txt", 7273)),
   ( 68, mkSol (p68, return (), "6531031914842725")),
   ( 69, mkSol (p69, return 1000000, 510510)),
   ( 70, mkSol (p70, return 10000000, 8319823)),
   ( 71, mkSol (p71, return 1000000, 428570 % 999997)),
   ( 72, mkSol (p72, return 1000000, 303963552391)),
   ( 73, mkSol (p73, return 12000, 7295372)),
   ( 74, mkSol (p74, return 999999, 402)),
   ( 75, mkSol (p75, return 1500000, 161667)),
   ( 76, mkSol (p31 [1..99], return 100, 190569291)),
   ( 77, mkSol (p77, return 5000, (71, 5007))),
   ( 78, mkSol (p78, return 1000000, 55374)),
   ( 79, mkSol (p79, Str <$> readFile "inputs/p79.txt", "73162890")),
   ( 80, mkSol (p80, return 100, 40886)),
   ( 81, mkSol (p81, Str <$> readFile "inputs/p81.txt", 427337)),
   ( 82, mkSol (p82, Str <$> readFile "inputs/p82.txt", 260324)),
   ( 83, mkSol (p83, Str <$> readFile "inputs/p83.txt", Just 425185)),
   ( 84, mkSol (p84, return 100000, ["10","15","24"])),
   ( 85, mkSol (p85, return 2000000, 2772)),
   ( 86, mkSol (p86, return 1000000, 1818)),
   ( 87, mkSol (p87, return 50000000, 1097343)),
   ( 88, mkSol (p88, return 12000, 7587457)),
   ( 89, mkSol (p89, Str <$> readFile "inputs/p89.txt", 743)),
   ( 90, mkSol (p90, return (), 1217))]

mayFile :: FilePath -> IO (Maybe String)
mayFile fp = do
    res <- Ex.try (readFile fp) :: IO (Either Ex.SomeException String)
    case res of
         Right contents -> return $ Just contents
         Left _         -> return Nothing

getInput :: String -> IO (Maybe String)
getInput i = return number <|> mayFile i <|> return (Just i)
    where
    number = show <$> (readMay i :: Maybe Int)
    
handle :: [String] -> IO ()
handle (a:_) = case Map.lookup p solutions of
        Just s  -> s >>= printSolution p
        Nothing -> putStrLn "Problem not yet solved !!!"
    where
    p = read a :: Int

handle [] = mapM_ (\(n, s) -> s >>= printSolution n) $ Map.toList solutions

main :: IO ()
main = do
    args <- getArgs
    handle args
