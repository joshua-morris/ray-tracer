module Hittable where

import Ray
import Vec3

data HitRecord = HitRecord { p :: Vec3 Double, normal :: Vec3 Double, t :: Double }

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord