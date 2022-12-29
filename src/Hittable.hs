module Hittable where

import Ray
import Types

data HitRecord = HitRecord { p :: Vec3, normal :: Vec3, t :: Float, frontFace :: Bool }

class Hittable a where
    hit :: a -> Ray -> Float -> Maybe Float -> Maybe HitRecord

minDistance :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
minDistance a Nothing = a
minDistance Nothing b = b
minDistance (Just a) (Just b) = if (t a) < (t b) then Just a else Just b

hitAll :: Hittable a => [a] -> Ray -> Float -> Maybe Float -> Maybe HitRecord
hitAll hl ray tmin tmax = foldr minDistance Nothing $ map (\x -> hit x ray tmin tmax) hl