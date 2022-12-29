module Rays.Util where

import Rays.Types
import Linear

clamp :: Ord a => a -> a -> a -> a
clamp x lb ub = min ub (max x lb)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v ^-^ 2*(dot v n)*^n