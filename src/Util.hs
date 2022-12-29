module Util where

clamp :: Ord a => a -> a -> a -> a
clamp x lb ub = min ub (max x lb)