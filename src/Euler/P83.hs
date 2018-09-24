module Euler.P83 (p83) where

import Euler.Util
import Control.Monad
import Data.Maybe
import Data.List.Split
import Data.Graph.AStar
import qualified Data.HashSet as HS

p83 :: Solution
p83 input = show score
    where
    matrix = (map (map read . splitOn ",") . lines) input :: [[Integer]]
    maxel = maximum $ concat matrix
    lut (x, y) = safeLu y matrix >>= safeLu x
    neigh (x, y) = filter (isJust . lut) $ map (\(x', y') -> (x' + x, y' + y)) l
        where l = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    graph p = HS.fromList $ neigh p
    start = (0, 0)
    goal@(gx, gy) = ((length . last) matrix - 1, length matrix - 1)
    path = (:) <$> Just start <*> aStar
                    graph
                    (\a b    -> fromMaybe (maxel + 1) $ (+) <$> lut a <*> lut b)
                    (\(x, y) -> toInteger $ (gx - x) + (gy - y))
                    (== goal)
                    start
    score = sum <$> join (mapM lut <$> path)
