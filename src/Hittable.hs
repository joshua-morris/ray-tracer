{-# LANGUAGE TemplateHaskell #-}

module Hittable where

import Control.Lens

import Ray
import Types

data HitRecord = HitRecord 
  { _p :: Vec3
  , _normal :: Vec3
  , _t :: Float
  , _frontFace :: Bool 
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