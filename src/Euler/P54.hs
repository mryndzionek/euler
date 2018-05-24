module Euler.P54 (p54) where

import Euler.Util
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Rank a = HighCard a | OnePair a | TwoPairs a |
              ThreeOfAKind a | Straight a | Flush a |
              FullHouse a | FourOfAKind a | StraightFlush a | RoyalFlush a deriving (Show, Eq, Ord)

p54 :: Solution
p54 input = show.length $ filter (\(p1, p2) -> score p1 > score p2) hands
    where
        hands =   map
                    ((\ (p1, p2) -> ((values p1, suits p1), (values p2, suits p2))) .
                       (splitAt 5 . splitOn " "))
                    (lines input)
        values = map (!! 0) :: [String] -> String
        suits  = map (!! 1) :: [String] -> String

trans :: Char -> Char
trans a = fromMaybe a (Map.lookup a m)
    where
        m = Map.fromList [('T', 'A'), ('J', 'B'), ('Q', 'C'), ('K', 'D'), ('A', 'E')]   
 
is :: String -> String -> Bool
is a b = Set.fromList a == Set.fromList b

isSame :: String -> Bool
isSame a = all (== head a) (tail a)

consecutive :: String -> Bool
consecutive a = sort' a `isInfixOf` "23456789ABCDE"
    where
        sort' b = sort $ map trans b

same :: String -> Int -> Bool
same a n = Set.member n $ Set.fromList $ map snd $ count a

twoPairs :: String -> Bool
twoPairs a = ([1, 2, 2] :: [Int]) == sort (map snd $ count a)

score :: (String, String) -> Rank String
score (values, suits)
    | values `is` "TJQKA" && isSame suits = RoyalFlush values'
    | consecutive values && isSame suits  = StraightFlush values'
    | same values 4                       = FourOfAKind (flt values' 4)
    | same values 3 && same values 2      = FullHouse (flt values' 2 ++ flt values' 3)
    | isSame suits                        = Flush values'
    | consecutive values                  = Straight values'
    | same values 3                       = ThreeOfAKind (flt values' 4)
    | twoPairs values                     = TwoPairs (flt values' 2)
    | same values 2                       = OnePair (flt values' 2)
    | otherwise                           = HighCard values'
        where
            flt :: String -> Int -> String
            flt v c = map fst $ filter (\a -> snd a == c) $ count v
            values' = sortBy (flip compare) (map trans values)
