module Hittable where

import Ray

data HitRecord = HitRecord { p :: Vec3, normal :: Vec3, t :: Double }

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> HitRecord