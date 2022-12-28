module Vec3 where

import Data.Functor
import Data.Foldable

data Vec3 a = Vec3 { vx, vy, vz :: a } deriving (Eq, Show)

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Foldable Vec3 where
  foldr f n (Vec3 x y z) = x `f` (y `f` (z `f` n))

vzip :: (a -> a -> a) -> Vec3 a -> Vec3 a -> Vec3 a
vzip f (Vec3 r1 g1 b1) (Vec3 r2 g2 b2) = Vec3 (f r1 r2) (f g1 g2) (f b1 b2)

vpromote :: a -> Vec3 a
vpromote x = Vec3 x x x

instance Num a => Num (Vec3 a) where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger x = vpromote (fromInteger x)

instance Fractional a => Fractional (Vec3 a) where
  (/) = vzip (/)
  recip = fmap recip
  fromRational x = vpromote (fromRational x)

clip :: (Num n, Ord n) => n -> n
clip n
  | n < 0 = 0
  | n > 1 = 1
  | otherwise = n

vclip :: (Num a, Ord a) => Vec3 a -> Vec3 a
vclip = fmap clip

vzero :: Num a => Vec3 a
vzero = Vec3 0 0 0

vscale :: Num a => a -> Vec3 a -> Vec3 a
vscale k = fmap (*k)

vlength :: (Num a, Fractional a, Floating a) => Vec3 a -> a
vlength = sqrt . sum . fmap (**2)

vdot :: Num a => Vec3 a -> Vec3 a -> a
vdot (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = x1*y1 + x2*y2 + x3*y3

vcross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
vcross (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x2*y3-x3*y2) (x3*y1-x1*y3) (x1*y2-x2*y1)

vnormalise :: (Num a, Fractional a, Floating a) => Vec3 a -> Vec3 a
vnormalise v = vscale (1/(vlength v)) v