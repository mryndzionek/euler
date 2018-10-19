module Euler.P84 (p84) where

import Data.List (sortBy)
import Text.Printf (printf)
import Euler.Util
import System.Random
import Control.Arrow (first, second)
import Control.Monad.State
import qualified Data.Map.Strict as Map

data Square =  GO | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3 | JAIL | 
               C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3 | FP | E1 | CH2 |
               E2 | E3 | R3 | F1 | F2 | U2 | F3 | G2J | G1 | G2 | CC3 | G3 |
               R4 | CH3 | H1 | T2 | H2 deriving (Show, Eq, Ord, Enum)

board :: [Square]
board = [GO .. H2]

cycleS :: Square -> [Square]
cycleS s = drop 1 $ dropWhile (/= s) $ cycle board

goBack :: Int -> Square -> Square
goBack n s = last $ take (length board - (n `rem` length board)) $ cycleS s

goNext :: [Square] -> Square -> Square
goNext ss s = head $ dropWhile (`notElem` ss) $ cycleS s

goR :: Square -> Square
goR = goNext [R1, R2, R3, R4]

goU :: Square -> Square
goU = goNext [U1, U2]

cc :: [Square -> Square]
cc = cycle $ map const [GO, JAIL] ++ replicate 14 id

ch :: [Square -> Square]
ch =  cycle $ goR : goR : goU : goBack 3 :
              map const [GO, JAIL, C1, E3, H2, R1] ++ replicate 6 id

update :: Square -> State ([Square -> Square], [Square -> Square]) Square
update s
    | s `elem` [CC1, CC2, CC3] = modify (first tail) >>
                                 get >>= \(cc', _) -> return . head cc' $ s
    | s `elem` [CH1, CH2, CH3] = modify (second tail) >>
                                 get >>= \(_, ch') -> return . head ch' $ s
    | s == G2J = return JAIL
    | otherwise = return s

run :: State (Square, ([Square -> Square], [Square -> Square]), Map.Map Square Integer, StdGen) ()
run = modify mdf
    where mdf (pos, c, m, g) = let score = sum $ take 2 $ randomRs (1, 4) g :: Int
                                   (sq, nc) = runState (update (last . take score $ cycleS pos)) c
                               in (sq, nc, Map.insertWith (\_ ov -> ov + 1) sq 1 m, mkStdGen . head $ randoms g)

p84 :: Solution
p84 input = concat . take 3 $ map (toStr . fromEnum . fst) $
            sortBy (\a b -> compare (snd b) (snd a)) $ Map.toList stats
        where
        limit = read input :: Int
        act = replicateM_ limit run
        toStr = printf "%02d" :: (Int -> String)
        (_, _, stats, _) = execState act (GO, (cc, ch), Map.empty, mkStdGen 666)