module Ray where

import Types
import Linear

data Ray = Ray { orig, dir :: Vec3 } deriving (Eq, Show)

rayAt :: Ray -> Float -> Vec3
rayAt (Ray orig dir) t = orig + t *^ dir