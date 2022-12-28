module Hittable where

import Ray
import Vec3

data HitRecord = HitRecord { p :: Vec3 Double, normal :: Vec3 Double, t :: Double }

class Hittable a where
    hit :: a -> Ray -> Double -> Maybe Double -> Maybe HitRecord

minDistance :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
minDistance a Nothing = a
minDistance Nothing b = b
minDistance (Just a) (Just b) = if (t a) < (t b) then Just a else Just b

hitAll :: Hittable a => [a] -> Ray -> Double -> Maybe Double -> Maybe HitRecord
hitAll hl ray tmin tmax = foldr minDistance Nothing $ map (\x -> hit x ray tmin tmax) hl