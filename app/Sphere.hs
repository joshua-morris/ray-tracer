module Sphere where

import Vec3
import Hittable

data Sphere = Sphere { center :: Vec3, radius :: Double }

instance Hittable Sphere where
    hit s r tmin tmax hr = undefined -- TODO