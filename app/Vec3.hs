module Trace.Vec3 where

data Vec3 = Vec3 { x, y, z :: Double } deriving (Eq, Show)

vmap :: (Double -> Double) -> Vec3 -> Vec3
vmap f (Vec3 r g b) = Vec3 (f r) (f g) (f b)

vzip :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
vzip f (Vec3 r1 g1 b1) (Vec3 r2 g2 b2) = Vec3 (f r1 r2) (f g1 g2) (f b1 b2)

vfold :: (Double -> Double -> Double) -> Vec3 -> Double
vfold f (Vec3 r g b) = r `f` g `f` b

vpromote :: Double -> Vec3
vpromote x = Vec3 x x x

instance Num Vec3 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  negate = vmap negate
  abs    = vmap abs
  signum = vmap signum
  fromInteger x = vpromote (fromInteger x)

instance Fractional Vec3 where
  (/) = vzip (/)
  recip = vmap recip
  fromRational x = vpromote (fromRational x)

clip :: (Num n, Ord n) => n -> n
clip n
  | n < 0 = 0
  | n > 1 = 1
  | otherwise = n

vclip :: Vec3 -> Vec3
vclip = vmap clip

vzero :: Vec3
vzero = Vec3 0 0 0

vscale :: Double -> Vec3 -> Vec3
vscale k (Vec3 x y z) = Vec3 (k*x) (k*y) (k*z)

vlength :: Vec3 -> Double
vlength (Vec3 x y z) = sqrt (x**2 + y**2 + z**2)

vdot :: Vec3 -> Vec3 -> Double
vdot (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = x1*y1 + x2*y2 + x3*y3

vcross :: Vec3 -> Vec3 -> Vec3
vcross (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x2*y3-x3*y2) (x3*y1-x1*y3) (x1*y2-x2*y1)

vnormalise :: Vec3 -> Vec3
vnormalise v = vscale (1/(vlength v)) v

vx (Vec3 x _ _) = x
vy (Vec3 _ y _) = y
vz (Vec3 _ _ z) = z