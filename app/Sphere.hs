module Sphere where

import Types
import Ray
import Hittable
import Linear

data Sphere = Sphere { center :: Vec3, radius :: Float }

discriminant :: Sphere -> Ray -> Maybe Float
discriminant sphere ray = if d < 0 then Nothing else Just d
    where
        oc = (orig ray) - (center sphere)
        half_b = dot oc (dir ray)
        a = (norm (dir ray))^2
        c = (norm oc)^2 - (radius sphere)^2
        d = half_b^2 - a*c


isInRange :: Float -> Float -> Maybe Float -> Bool
isInRange root tmin Nothing = root >= tmin
isInRange root tmin (Just tmax) = root >= tmin && root <= tmax

nearestRoot :: Sphere -> Ray -> Float -> Maybe Float -> Maybe Float
nearestRoot sphere ray tmin tmax = case discriminant sphere ray of
    Nothing -> Nothing
    Just d -> if isInRange root1 tmin tmax
                then Just root1 
              else if isInRange root2 tmin tmax
                then Just root2
              else Nothing
        where
            oc = (orig ray) - (center sphere)
            root1 = (-half_b - (sqrt d)) / a
            root2 = (-half_b + (sqrt d)) / a
            a = (norm (dir ray))**2
            half_b = dot oc (dir ray)
            c = (norm oc)**2 - (radius sphere)**2

instance Hittable Sphere where
    hit sphere ray tmin tmax = case nearestRoot sphere ray tmin tmax of
        Nothing -> Nothing
        Just root -> Just (HitRecord p normal root frontFace)
            where
                normal = if frontFace then outwardNormal else -outwardNormal
                frontFace = (dot (dir ray) outwardNormal) < 0
                outwardNormal = (1/(radius sphere)) *^ (p - (center sphere))
                p = rayAt ray root