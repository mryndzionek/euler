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
import Control.Monad.Trans.Maybe
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
p7 input = show (primes !! upper :: Integer)
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
p14 input = show.snd.maximum $ map ((length . collatz) &&& (head . collatz)) [1..size]
    where
    size = read input - 1 :: Int
    collatz n = takeWhile (> 1) (iterate next n) ++ [1]
    next x
        | even x  = x `quot` 2
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
    size = read input :: Int
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
        lc = (y - 1) `quot` 4

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
p24 input = concatMap show (Per.elems . last $ take size perm)
    where
    size = read input :: Int
    perm = perm' $ Just $ Per.permute 10
    perm' p = case p of
                   Just p' -> p' : perm' (Per.next p')
                   Nothing -> []

p25 :: Solution
p25 input = show $ (+1).length $ takeWhile (\a -> length (show a) < size) (1:fibs)
    where
    size = read input :: Int

p26 :: Solution
p26 input = show.snd.maximum $ zip recuring ([1..] :: [Int])
    where
    limit = read input :: Int
    recur n = unfoldr (\a -> Just (a `rem` n, 10 * (a `rem` n))) 10
    recuring = [length.Set.fromList $ take limit $ recur n | n <- [1..limit]] :: [Int]

p27 :: Solution
p27 input = show.product.snd.maximum $ [(plen a b, [a, b]) | a <- [-limit..limit], b <- [-limit..limit]]
    where
    plen a b = length $ takeWhile isPrime [q | n <- [0..], let q = n * n + a * n + b, q >= 0]
    limit = read input :: Int

p28 :: Solution
p28 input = show.sum.concat $ diagonals
    where
    size = read input :: Integer
    maxLevel = (size - 1) `quot` 2
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

p31 :: [Int] -> Solution
p31 coins input = show $ ways amount coins
    where
    amount = read input :: Int
    ways :: Int -> [Int] -> Integer
    ways n c = last . snd $ until (null . fst) way (c, 1 : replicate n 0)
    way (c, s) = (drop 1 c, zipWith (+) s (replicate (head c) 0 ++ snd (way (c, s))))

p32 :: Solution
p32 input = show.sum.nub $ map (uncurry (*)) candidates
    where
    limit = read input :: Int
    candidates = filter (\(n,m) -> isPandigit $ toStr (n, m))
                 [(n, m) | n <- [1..limit - 1], m <- [1..limit `quot` n]] :: [(Int, Int)]
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
    iscircular n = all isPrime $ rotations n
    rotations :: Int -> [Int]
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
p37 _ = show.sum $ take 11 $ dropWhile (<8) (filter isTrunc primes :: [Integer])
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

p38 :: Solution
p38 _ = maximum $ filter isPandigit candidates
    where
    candidates = concatMap candidate ranges
    candidate a = takeWhile ((<10).length) $ scanl1 (++) $ map (show . (a *)) [1 ..]
    ranges = concat [[99, 98..91], [999, 998..918], [9999, 9998..9182]] :: [Integer]
    isPandigit n = let m = filter (/= '0') n in
                           (length m == 9) && length m == length (Set.fromList m)

p39 :: Solution
p39 input = (show.snd.maximum) [(length $ triplets p, p) | p <- [2..limit]]
    where
    limit = read input :: Int
    triplets p = [(a, b, c) | a <- [2..p `quot` 3],
                            let b = p * (p - 2 * a) `quot` (2 * (p - a)),
                            let c = p - a - b,
                            b > a,
                            p * (p - 2 * a) `rem` (2 * (p - a)) == 0]

p40 :: Solution
p40 input = (show.product) [d (10^a) | a <- [0..limit]]
    where
    limit = read input :: Int
    d n = digitToInt.last $ take n (concatMap show ([1..] :: [Int]))

p41 :: Solution
p41 _ = show.maximum $ filter isPrime $ concatMap pandigits [4, 7]
    where
    pandigits :: Int -> [Integer]
    pandigits n = map read $ permutations [intToDigit a | a <- [1..n]]

p42 :: Solution
p42 input = show.length $ filter (`Set.member` triangles) scores
    where
    scores = map wordToNum names
    triangles = Set.fromList $ takeWhile (<=maximum scores) [n * (n + 1) `quot` 2 | n <- [1..]]
    wordToNum w = sum $ map (\x -> ord x - 64) w
    strip = dropWhileEnd (=='"') . dropWhile (=='"')
    names = map strip $ splitOn "," $ concat $ lines input

p43 :: Solution
p43 _ = show.sum $ (map (read . map intToDigit) found :: [Int])
    where
    found = filter test [p | p <- permutations [0..9], p !! 5 == 5]
    test p = all ((==0).uncurry mod) $ zip (subs p) (primes :: [Integer])
    subs p = map (read . (map intToDigit . (\ x -> (\ y z -> take 3 $ drop y z) x p))) [1 .. 7]

p44 :: Solution
p44 _ = show.head $ candidates
    where
    pentagonals = [(n*(3*n - 1)) `quot` 2 | n <- [1..]]
    isPentagonal n = v == fromInteger (round v) && (round v `rem` 6 :: Int) == 0
        where
        v = isqrt (1 + 24 * n) + 1
        isqrt = (sqrt . fromIntegral) :: Int -> Double
    candidates = [j - k | j <- pentagonals, k <- takeWhile (< j) pentagonals,
                      isPentagonal (j - k), isPentagonal (j + k)]

p45 :: Solution
p45 input = show.head $ [t | t <- triangles, isPentagonal t, isHexagonal t]
    where
    start = read input :: Int
    triangles = [n * (n + 1) `quot` 2 | n <- [start..]]
    isqrt = (sqrt . fromIntegral) :: Int -> Double
    isPentagonal n = v == fromInteger (round v) && (round v `rem` 6 :: Int) == 0
        where
        v = isqrt (1 + 24 * n) + 1
    isHexagonal n = v == fromInteger (round v) && (round v `rem` 4 :: Int) == 0
        where
        v = isqrt (1 + 8 * n) + 1

p46 :: Solution
p46 _ = show.head $ dropWhile check ([9,11..] :: [Integer])
    where
    check n = any isTwice $ map (n-) $ takeWhile (<= n) primes
    isTwice m = v == fromInteger (round v)
        where
        v = sqrt (fromIntegral m / 2) :: Double

p47 :: Solution
p47 input = get' $ filter check [map facts [a..a + len - 1] | a <- [0..]]
    where
    get' = show.product.head.head
    len = read input :: Int
    check p = all ((== len) . length) p && (m == nub m)
        where
            m = concat p
    facts n = map (uncurry (*)) $ (count.pfactors) n

p48 :: Solution
p48 input = show.last10digits $ sum [n ^ n | n <- [1..limit]]
    where
    last10digits n = n `rem` (10^(10 :: Integer))
    limit = read input :: Integer

p49 :: Solution
p49 _ = show $ flip (!!) 1 $ do
    candidate <- takeWhile (<10000) $ dropWhile (<999) (primes :: [Integer])
    step <- [1..9999]
    let candidates = [candidate, candidate + step, candidate + 2 * step]
    let sets = map (Set.fromList.show) candidates
    guard $ candidate + 2 * step < 10000
    guard $ all isPrime candidates
    guard $ all (== head sets) sets
    return $ concatMap show candidates

p50 :: Solution
p50 input = show.maximum $ filter (isPrime.snd) $ concatMap gen [take n sums | n <- [1..length sums]]
    where
    limit = read input :: Int
    gen n = zip [length n, length n - 1..] (map (last n -) (0:n))
    sums = takeWhile (<limit) cumul
    cumul = scanl1 (+) primes

p52 :: Solution
p52 input = show.head $ do
    n <- [1..] :: [Int]
    let candidates = map (show.(*) n) [1..upTo]
    guard $ all (== Set.fromList (head candidates)) $ map Set.fromList (tail candidates)
    return n
        where
         upTo = read input :: Int

p53 :: Solution
p53 input = show.length $ filter (>limit) $ map combi perms
    where
    limit = read input :: Integer
    factorials = 1 : map (\n -> product [1..n]) [1..]
    combi (r, n) = factorials !! n `quot` (factorials !! r * factorials !! (n - r))
    perms = [(n, x) | x <- [1..100], n <- [1..x]]

p55 :: Solution
p55 input = show.length $ filter (not . any isPalindr . candidates) [1..limit]
    where
    limit = read input :: Integer
    rev = read . reverse . show
    isPalindr n = n == rev n
    candidates n = take 50 . drop 1 $ iterate (\x -> x + rev x) n

p56 :: Solution
p56 input = show . maximum $ map digitSum [a ^ b | a <- [1..limit], b <- [1..limit]]
    where
    limit = read input :: Integer
    digitSum = sum . map digitToInt . show

p57 :: Solution
p57 input = show . length $ filter isLonger $ take limit expansion
    where
    limit = read input :: Int
    isLonger :: Ratio Integer -> Bool
    isLonger n = length (show $ numerator n) > length (show $ denominator n)
    expansion = iterate (\x -> 1 + 1 / (1 + x)) 1

p58 :: Solution
p58 input = show.fst.head $ dropWhile ((>ratio).snd) $ zip ([3,5..] :: [Integer]) $ zipWith (%) prime (5 : [9,13..])
    where
    ratio = read input % 100 :: Ratio Int
    diagonals = [take 4 $ iterate (\x -> x - 2 * l) s |
                       l <- [1..], let a = 2 * l + 1,
                                   let s = a * a] :: [[Integer]]
    prime = drop 1 $ scanl (\x y -> x + length (filter isPrime y)) 0 diagonals

p59 :: Solution
p59 input = show $ sum $ zipWith xor ciphertxt (cycle key)
    where
    ciphertxt = map read $ splitOn "," input
    chunks = transpose $ chunksOf 3 ciphertxt
    maxCount :: [Int] -> (Integer, Int)
    maxCount x = maximum $ map swap $ count x
    key = map ((xor (ord ' ') . snd) . maxCount) chunks

p63 :: Solution
p63 _ = show . sum $ ([floor(1 / ((1 :: Double) - logBase 10 n)) | n <- [1..9]] :: [Integer])

p69 :: Solution
p69 input = show . last $ takeWhile (<limit) $ map product [take n primes | n <- [1..]]
    where
        limit = read input :: Integer

p70 :: Solution
p70 input = show . snd $ minimum ([(divi n phi, n) | (n, phi) <- candidates, perm n phi] :: [(Double, Integer)])
    where
        limit = read input :: Integer
        divi a b = fromIntegral a / fromIntegral b
        perm x y = sort (show x) == sort (show y)
        totient p_1 p_2 = (p_1 - 1) * (p_2 - 1)
        candidates = [(n, phi) | p_1 <- ps, p_2 <- dropWhile (<= p_1) ps,
                                 let n = p_1 * p_2, n <= limit,
                                 let phi = totient p_1 p_2]
            where ps = takeWhile (<=2 * pl) primes
                  pl = head $ dropWhile ((<=limit) . (^(2 :: Integer))) primes

p71 :: Solution
p71 input = show . fst $ until (\(x, y) -> denominator (mediant x y) > limit) farey (0, 1)
    where
    limit = read input :: Integer
    farey (a, b) = let m = mediant a b in if m < (3 % 7) then (m, b) else (a, m)
    mediant a b = (numerator a + numerator b) % (denominator a + denominator b)

p72 :: Solution
p72 input = show $ fareySum limit - 2
    where
    limit = read input :: Int
    fareySum m = map f [0..] !! m
        where
        f n = (n*(n + 3)) `quot` 2 - sum [fareySum (n `quot` k) | k <- [2..n]]

p73 :: Solution
p73 input = show . length $ farey limit
    where
    limit = read input :: Int
    farey l = drop 1 $ iFarey (1 % 3, 1 % 2)
        where
        iFarey (a, b)
            | denominator mediant > l = [a]
            | otherwise = iFarey (a, mediant) ++ iFarey (mediant, b)
            where
            mediant = (numerator a + numerator b) % (denominator a + denominator b)


p74 :: Solution
p74 input = show . length $ filter (==60) $ map (length . chain) [1..limit]
    where
    limit = read input :: Int
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

p75 :: Solution
p75 input = show . length $ filter ((==(1 :: Integer)) . snd) $ count $ do
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
        where
        limit = read input :: Integer

p77 :: Solution
p77 input = show . head $ dropWhile ((< limit) . read . snd) candidates
    where
    candidates = [(last p, p31 p (show (last p))) | n <- [1..], let p = take n primes]
    limit = read input :: Integer

p78 :: Solution
p78 input = show . head $ [i | (i, p) <- assocs lut, p `rem` limit == 0]
    where
    limit = read input :: Integer
    pentagonals = sort [n * (3 * n - 1) `quot` 2 | n <- [-250..250], n /= 0]
    terms m = zip (cycle [1, 1, -1, -1]) (takeWhile (<= m) pentagonals)
    lut :: Array Integer Integer
    lut = array (0, limit) [(x, part x) | x <- [0..limit]]
    part n | n <  0 = 0
           | n == 0 = 1
           | otherwise = sum [s * (lut ! (n - p)) | (s, p) <- terms n]

p79 :: Solution
p79 keylog = concatMap (show . snd) $ sort' $ map (\(a, b) -> ((length . nub) b, a)) prec
    where
    attempts = map (map digitToInt ) $ lines keylog
    prec  = Map.toList $ Map.fromListWith (++) $ concatMap assoc attempts
    sort' = sortBy (flip compare)
    assoc = unfoldr f
        where   
        f [] = Nothing
        f (x:xs) = Just ((x, xs), xs)

p80 :: Solution
p80 input = show.sum $ concatMap (sqrtDigits 99) [n | n <- [1..limit], not $ isSquare n]
    where
    limit = read input :: Integer
    isSquare :: Integer -> Bool
    isSquare n = let s = ((floor :: Double -> Integer).sqrt.fromIntegral) n in (s * s) == n
    isqrt 0 = 0
    isqrt 1 = 1
    isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)
    digits = map digitToInt . show :: Integer -> [Int]
    sqrtDigits :: Int -> Integer -> [Int]
    sqrtDigits c x = digits $ isqrt $ x * (10 ^ (2 * c))

p81 :: Solution
p81 input = show.head $ foldr fld (scanr1 (+) $ last matrix) (init matrix)
        where
        matrix = (map (map read . splitOn ",") . lines) input
        fld :: [Integer] -> [Integer] -> [Integer]
        fld cur prev = init $ scanr (\(p, c) a -> c + min a p) (last prev) (zip prev cur)

p82 :: Solution
p82 input = show.minimum $ foldr1 fld1 matrix
        where
        matrix = transpose $ (map (map read . splitOn ",") . lines) input
        fld1 :: [Integer] -> [Integer] -> [Integer]
        fld1 c p = let x = fld c p
                       y = fld (reverse c) (reverse p)
                    in zipWith min x (reverse y)
        fld :: [Integer] -> [Integer] -> [Integer]
        fld c' p' = tail $ scanl (\a (p, c) -> c + min a p) (head p') (zip p' c')

p85 :: Solution
p85 input = let limit = read input :: Integer
                asum x = x * (x + 1) `div` 2
            in show . snd $ minimum [(abs (asum x * asum y - limit), x * y) | x <- [1 .. 2000], y <- [1 .. 2000]]

p86 :: Solution
p86 input = show . fst . head $ dropWhile ((<limit) . snd) isum
        where
            limit = read input :: Integer
            candidates = [(l, wh) | l <- [3 .. ], wh <- [3 .. 2 * l],
                            let s = (sqrt . fromInteger) (wh * wh + l * l),
                            s == (fromInteger (round s) :: Double)]
            nsol l wh = if wh <= l then floor (halve wh)
                                   else 1 + l - ceiling (halve wh)
                where
                    halve :: Integer -> Double
                    halve a = fromIntegral a / 2
            isum = scanl' (\(_, s) (l, wh) -> (l, s + nsol l wh)) (0, 0) candidates

p87 :: Solution
p87 input = show . length . Set.fromList . filter (< limit) $ nums
    where
    limit = read input :: Int
    plimit = floor (sqrt (fromIntegral limit) :: Double) :: Integer
    primes' = map fromIntegral $ takeWhile (<= plimit) primes
    f a b c = (a ^ (2 :: Int)) + (b ^ (3 :: Int)) + (c ^ (4 :: Int))
    nums = f <$> primes' <*> primes' <*> primes'

p88 :: Solution
p88 input = show . sum . nub . drop 2 . Map.elems $ Map.foldlWithKey f'' accum cache
        where
        limit = read input :: Int
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

p89 :: Solution
p89 input = show . sum $ map (sum . map gain . romCut) literals
    where
    literals = lines input
    lut = ["I", "IIII", "IV", "V", "VIIII", "IX", "X", "XXXX", "XL", "L", "LXXXX", "XC", "C", "CCCC", "CD", "D", "DCCCC", "CM", "M"]
    romCut = unfoldr (\l -> let p = last . filter (`isPrefixOf` l) $ lut
                            in if null l then Nothing else Just (p, drop (length p) l))
    gain :: String -> Int
    gain l
        | l `elem` ["VIIII", "DCCCC", "LXXXX"] = 3
        | l `elem` ["IIII", "XXXX", "CCCC"] = 2
        | otherwise = 0

solutions :: Map.Map Int (Solution, IO String)
solutions = Map.fromList [
   (  1, ( p1, return "1000")),
   (  2, ( p2, return "4000000")),
   (  3, ( p3, return "600851475143")),
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
   ( 31, (p31 [1, 2, 5, 10, 20, 50, 100, 200], return "200")),
   ( 32, (p32, return "10000")),
   ( 33, (p33, return "")),
   ( 34, (p34, return "")),
   ( 35, (p35, return "1000000")),
   ( 36, (p36, return "1000000")),
   ( 37, (p37, return "")),
   ( 38, (p38, return "")),
   ( 39, (p39, return "1000")),
   ( 40, (p40, return "6")),
   ( 41, (p41, return "")),
   ( 42, (p42, readFile "inputs/p42.txt")),
   ( 43, (p43, return "")),
   ( 44, (p44, return "")),
   ( 45, (p45, return "286")),
   ( 46, (p46, return "")),
   ( 47, (p47, return "4")),
   ( 48, (p48, return "1000")),
   ( 49, (p49, return "")),
   ( 50, (p50, return "1000000")),
   ( 51, (p51, return "6")),
   ( 52, (p52, return "6")),
   ( 53, (p53, return "1000000")),
   ( 54, (p54, readFile "inputs/p54.txt")),
   ( 55, (p55, return "9999")),
   ( 56, (p56, return "99")),
   ( 57, (p57, return "1000")),
   ( 58, (p58, return "10")),
   ( 59, (p59, readFile "inputs/p59.txt")),
   ( 60, (p60, return "10000")),
   ( 61, (p61, return "9999")),
   ( 62, (p62, return "")),
   ( 63, (p63, return "")),
   ( 64, (p64, return "10000")),
   ( 65, (p65, return "100")),
   ( 66, (p66, return "1000")),
   ( 67, (p18, readFile "inputs/p67.txt")),
   ( 68, (p68, return "")),
   ( 69, (p69, return "1000000")),
   ( 70, (p70, return "10000000")),
   ( 71, (p71, return "1000000")),
   ( 72, (p72, return "1000000")),
   ( 73, (p73, return "12000")),
   ( 74, (p74, return "999999")),
   ( 75, (p75, return "1500000")),
   ( 76, (p31 [1..99], return "100")),
   ( 77, (p77, return "5000")),
   ( 78, (p78, return "1000000")),
   ( 79, (p79, readFile "inputs/p79.txt")),
   ( 80, (p80, return "100")),
   ( 81, (p81, readFile "inputs/p81.txt")),
   ( 82, (p82, readFile "inputs/p82.txt")),
   ( 83, (p83, readFile "inputs/p83.txt")),
   ( 84, (p84, return "100000")),
   ( 85, (p85, return "2000000")),
   ( 86, (p86, return "1000000")),
   ( 87, (p87, return "50000000")),
   ( 88, (p88, return "12000")),
   ( 89, (p89, readFile "inputs/p89.txt"))]

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
