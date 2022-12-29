module Rays.Ray where

import Rays.Types
import Linear

data Ray = Ray 
  { _orig
  , _dir :: Vec3 
  } deriving (Eq, Show)

rayAt :: Ray -> Float -> Vec3
rayAt (Ray orig dir) t = orig + t *^ dir