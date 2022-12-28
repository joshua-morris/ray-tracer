module Sphere where

import Vec3
import Ray
import Hittable

data Sphere = Sphere { center :: Vec3 Double, radius :: Double }

discriminant :: Sphere -> Ray -> Maybe Double
discriminant sphere ray = if d < 0 then Nothing else Just d
    where
        oc = (orig ray) - (center sphere)
        half_b = dot oc (dir ray)
        a = (vlength (dir ray))**2
        c = (vlength oc)**2 - (radius sphere)**2
        d = half_b*half_b - a*c
        

nearestRoot :: Sphere -> Ray -> Double -> Maybe Double -> Maybe Double
nearestRoot sphere ray tmin tmax = case discriminant sphere ray of
    Nothing -> Nothing
    Just d -> if root1 < tmin || root1max then Just root1 else Just root2
        where
            root1max = case tmax of
                Nothing -> False
                Just n -> root1 > n
            oc = (orig ray) - (center sphere)
            root1 = (-half_b - (sqrt $ d)) / a
            root2 = (-half_b + (sqrt $ d)) / a
            a = (vlength (dir ray))**2
            half_b = dot oc (dir ray)
            c = (vlength oc)**2 - (radius sphere)**2

instance Hittable Sphere where
    hit sphere ray tmin tmax = case nearestRoot sphere ray tmin tmax of
        Nothing -> Nothing
        Just root -> Just (HitRecord p normal root frontFace)
            where
                normal = if frontFace then outwardNormal else -outwardNormal
                frontFace = (dot (dir ray) outwardNormal) < 0
                outwardNormal = scale (1/(radius sphere)) (p - (center sphere))
                p = rayAt ray root