module Euler.P54 (p54) where

import Euler.Util
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as Set

p54 :: Solution
p54 input = show.length $ filter (uncurry compare') hands
    where
        hands =   map
                    ((\ (p1, p2) -> ((values p1, suits p1), (values p2, suits p2))) .
                       (splitAt 5 . splitOn " "))
                    (lines input)
        values = map (!! 0) :: [String] -> String
        suits  = map (!! 1) :: [String] -> String

trans :: Char -> Char
trans a
    | a <= '9' = a
    | a == 'T' = 'A'
    | a == 'J' = 'B'
    | a == 'Q' = 'C'
    | a == 'K' = 'D'
    | a == 'A' = 'E'
    | otherwise = undefined   
 
is :: String -> String -> Bool
is a b = Set.fromList a == Set.fromList b

isSame :: String -> Bool
isSame a = all (== head a) (tail a)

consecutive :: String -> Bool
consecutive a = sort' a `isInfixOf` "23456789ABCDE"
    where
        sort' b = sort $ map trans b

score :: String -> [Int]
score values = sortBy (flip compare) (map (ord . trans) values)

same :: String -> Int -> Bool
same a n = Set.member n $ Set.fromList $ map snd $ count a

twoPairs :: String -> Bool
twoPairs a = ([1, 2, 2] :: [Int]) == sort (map snd $ count a)

rank' :: (String, String) -> Int
rank' (values, suits)
    | values `is` "TJQKA" && isSame suits = 9
    | consecutive values && isSame suits  = 8
    | same values 4                       = 7
    | same values 3 && same values 2      = 6
    | isSame suits                        = 5
    | consecutive values                  = 4
    | same values 3                       = 3
    | twoPairs values                     = 2
    | same values 2                       = 1
    | otherwise                           = 0

filter' :: Int -> (String, String) -> String
filter' rank (values, _)
    | rank == 7 = flt values 4
    | rank == 3 = flt values 3
    | rank == 1 = flt values 2
    | rank == 2 = flt values 2
    | rank == 6 = flt values 2 ++ flt values 3
    | otherwise = values
        where
            flt :: String -> Int -> String
            flt v c = map fst $ filter (\a -> snd a == c) $ count v

compare' :: (String, String) -> (String, String) -> Bool
compare' p1 p2
    | r1 == r2 = score (filter' r1 p1) > score (filter' r2 p2)
    | otherwise = r1 > r2
    where
        r1 = rank' p1
        r2 = rank' p2
