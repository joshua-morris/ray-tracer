module Rays.Util where

import Rays.Types
import Linear

clamp :: Ord a => a -> a -> a -> a
clamp x lb ub = min ub (max x lb)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v ^-^ 2*(dot v n)*^n

refract :: Vec3 -> Vec3 -> Float -> Vec3
refract uv n eoe = rPerp + rPar
  where
    cosTheta = min (dot (-uv) n) 1
    rPerp = eoe*^(uv^+^cosTheta*^n)
    rPar = -(sqrt $ abs (1 - quadrance rPerp))*^n

reflectance :: Fractional a => a -> a -> a
reflectance cosine refIdx = r0 + (1-r0)*(1-cosine)^5
  where
    r0 = ((1-refIdx)/(1+refIdx))^2

degreesToRadians :: Float -> Float
degreesToRadians degrees = degrees * pi / 180