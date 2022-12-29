{-# LANGUAGE TemplateHaskell #-}

module Rays.Hittable where

import Control.Lens
import Linear

import Rays.Ray
import Rays.Random
import Rays.Types
import Rays.Material
import Rays.Util

data HitRecord = HitRecord 
  { _p :: Vec3
  , _normal :: Vec3
  , _t :: Float
  , _frontFace :: Bool
  , _material :: Material
  }

makeLenses ''HitRecord

class Hittable a where
    hit :: a -> Ray -> Float -> Maybe Float -> Maybe HitRecord

minDistance :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
minDistance a Nothing = a
minDistance Nothing b = b
minDistance (Just a) (Just b) = if a^.t < b^.t then Just a else Just b

hitAll :: Hittable a => [a] -> Ray -> Float -> Maybe Float -> Maybe HitRecord
hitAll hl ray tmin tmax = foldr minDistance Nothing $ map (\x -> hit x ray tmin tmax) hl

scatter :: Material -> Ray -> HitRecord -> RandomState (Maybe (Vec3, Ray))
scatter (Matte albedo) ray hr = do
    uv <- getRandomInUnitSphere
    let scatterDir = (hr^.normal) + uv
        scattered = Ray (hr^.p) scatterDir
    return $ Just (albedo, scattered)
scatter (Metal albedo fuzz) (Ray orig dir) hr = do
  uv <- getRandomInUnitSphere
  let reflected = reflect (normalize dir) (hr^.normal)
      scattered = Ray (hr^.p) (reflected+fuzz*^uv)
      attenuation = albedo
  return $ if (dot reflected (hr^.normal)) > 0 then Just (albedo, scattered) else Nothing