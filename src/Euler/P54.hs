module Euler.P54 (p54) where

import Euler.Util
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Value = Two   | Three | Four | Five | Six   | Seven |
             Eight | Nine  | Ten  | Jack | Queen | King  | Ace deriving (Show, Eq, Ord, Enum)

data Rank a = HighCard a | OnePair a | TwoPairs a |
              ThreeOfAKind a | Straight a    | Flush a         |
              FullHouse a    | FourOfAKind a | StraightFlush a | RoyalFlush a deriving (Show, Eq, Ord)

char2value :: Char -> Maybe Value
char2value a = Map.lookup a m
    where
    m = Map.fromList (zip "23456789TJQKA" [Two .. Ace])

p54 :: Str -> Maybe Int
p54 (Str input) = (length . filter (\(p1, p2) -> score p1 > score p2)) <$> hands
    where
        hands = mapM
                    ((\ (p1, p2) ->
                    do  v1 <- values p1
                        v2 <- values p2
                        return ((v1, suits p1), (v2, suits p2)))
                        . (splitAt 5 . splitOn " ")) (lines input)
        values = mapM (char2value . (!! 0)) :: [String] -> Maybe [Value]
        suits  = map (!! 1) :: [String] -> String  
 
is :: [Value] -> [Value] -> Bool
is a b = Set.fromList a == Set.fromList b

isSame :: String -> Bool
isSame a = all (== head a) (tail a)

consecutive :: [Value] -> Bool
consecutive a = sort a `isInfixOf` [Two .. Ace]

same :: [Value] -> Int -> Bool
same a n = Set.member n $ Set.fromList $ map snd $ count a

twoPairs :: [Value] -> Bool
twoPairs a = ([1, 2, 2] :: [Int]) == sort (map snd $ count a)

score :: ([Value], String) -> Rank [Value]
score (values, suits)
    | values `is` [Ten .. Ace] && isSame suits = RoyalFlush values'
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
            flt :: [Value] -> Int -> [Value]
            flt v c = map fst $ filter (\a -> snd a == c) $ count v
            values' = sortBy (flip compare) values
