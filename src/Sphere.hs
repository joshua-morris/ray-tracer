module Sphere where

import Types
import Ray
import Hittable
import Linear

data Sphere = Sphere !Vec3 !Float deriving (Eq)

discriminant :: Sphere -> Ray -> Maybe Float
discriminant (Sphere center radius) (Ray orig dir) = if d < 0 then Nothing else Just d
    where
        oc = orig - center
        half_b = dot oc dir
        a = (norm dir)^2
        c = (norm oc)^2 - radius^2
        d = half_b^2 - a*c


isInRange :: Float -> Float -> Maybe Float -> Bool
isInRange root tmin Nothing = root >= tmin
isInRange root tmin (Just tmax) = root >= tmin && root <= tmax

nearestRoot :: Sphere -> Ray -> Float -> Maybe Float -> Maybe Float
nearestRoot s@(Sphere center radius) ray@(Ray orig dir) tmin tmax = case discriminant s ray of
    Nothing -> Nothing
    Just d -> if isInRange root1 tmin tmax
                then Just root1 
              else if isInRange root2 tmin tmax
                then Just root2
              else Nothing
        where
            oc = orig - center
            root1 = (-half_b - (sqrt d)) / a
            root2 = (-half_b + (sqrt d)) / a
            a = (norm dir)**2
            half_b = dot oc dir
            c = (norm oc)**2 - radius**2

instance Hittable Sphere where
    hit s@(Sphere center radius) ray@(Ray orig dir) tmin tmax = case nearestRoot s ray tmin tmax of
        Nothing -> Nothing
        Just root -> Just (HitRecord p normal root frontFace)
            where
                normal = if frontFace then outwardNormal else -outwardNormal
                frontFace = (dot dir outwardNormal) < 0
                outwardNormal = (1/radius) *^ (p - center)
                p = rayAt ray root