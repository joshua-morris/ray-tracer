module Ray where

import Vec3

data Ray = Ray { orig, dir :: Vec3 } deriving (Eq, Show)

rayAt :: Ray -> Double -> Vec3
rayAt (Ray orig dir) t = orig + vscale t dir

lerp :: Double -> Vec3 -> Vec3 -> Vec3
lerp t u v = vscale (1-t) u + vscale t v

hitSphere :: Ray -> Vec3 -> Double -> Maybe Double
hitSphere ray center radius = if discriminant > 0 then Just ((-b - sqrt(discriminant)) / (2.0*a)) else Nothing
    where
        discriminant = b*b - 4*a*c
        a = vdot (dir ray) (dir ray)
        b = 2.0 * vdot oc (dir ray)
        c = vdot oc oc - radius*radius
        oc = (orig ray) - center