module Vec3 where

import Data.Functor
import Data.Foldable
import Control.Applicative

data Vec3 a = Vec3 { vx, vy, vz :: a } deriving (Eq, Show)

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
  pure a = Vec3 a a a
  (Vec3 f g h) <*> (Vec3 x y z) = Vec3 (f x) (g y) (h z)

instance Foldable Vec3 where
  foldr f n (Vec3 x y z) = x `f` (y `f` (z `f` n))

instance Num a => Num (Vec3 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger x = pure (fromInteger x)

instance Fractional a => Fractional (Vec3 a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational x = pure (fromRational x)

scale :: Num a => a -> Vec3 a -> Vec3 a
scale k = fmap (*k)

vlength :: (Num a, Fractional a, Floating a) => Vec3 a -> a
vlength = sqrt . sum . fmap (**2)

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = x1*y1 + x2*y2 + x3*y3

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x2*y3-x3*y2) (x3*y1-x1*y3) (x1*y2-x2*y1)

vnormalise :: (Num a, Fractional a, Floating a) => Vec3 a -> Vec3 a
vnormalise v = scale (1/(vlength v)) v

lerp :: Num a => a -> Vec3 a -> Vec3 a -> Vec3 a
lerp t u v = scale (1-t) u + scale t v