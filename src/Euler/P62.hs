module Euler.P62 (p62) where

import Euler.Util
import Data.List
import Control.Monad.State
import qualified Data.Map.Strict as Map

p62 :: Solution
p62 _ = show $ evalState (solve 1) Map.empty

solve :: Integer -> State (Map.Map String (Integer, Integer)) Integer
solve n = do
    m <- get
    case Map.lookup k m of
         Nothing ->
            do
            modify $ \m' -> Map.insert k (1, v) m'
            solve (n + 1)
         Just (c, v') ->
            if (c + 1) == 5 then return v' else
              (do modify $ \ m' -> Map.adjust (\ (x, y) -> (x + 1, min y v)) k m'
                  solve (n + 1))
    where
        v = n ^ (3 :: Integer)
        k = (sort . show) v
