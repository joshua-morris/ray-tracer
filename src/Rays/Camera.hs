{-# LANGUAGE TemplateHaskell #-}

module Rays.Camera where

import Control.Lens

import Rays.Util
import Rays.Types
import Linear
import Rays.Ray
import Rays.Random

data Camera = Camera 
  { _viewportHeight :: !Float
  , _viewportWidth :: !Float
  , _focalLength :: !Float 
  , _origin :: !Vec3
  , _horizontal :: !Vec3
  , _vertical :: !Vec3
  , _lowerLeftCorner :: !Vec3
  , _uu, _vv, _ww :: !Vec3
  , _lensRadius :: !Float
  } deriving (Eq,Show)

makeLenses ''Camera

cameraBuilder :: 
  Float 
  -> Float 
  -> Vec3 
  -> Vec3 
  -> Vec3 
  -> Float
  -> Float
  -> RandomState Camera
cameraBuilder vfov aspectRatio lookFrom lookAt vup aperture focusDist = do
  uv <- getRandomInUnitDisk
  let theta  = degreesToRadians vfov
      h      = tan (theta/2)
      height = 2*h
      width  = aspectRatio*height
      w'     = normalize (lookFrom - lookAt)
      u'     = normalize (cross vup w')
      v'     = cross w' u'
      horiz  = focusDist*width*^u'
      vert   = focusDist*height*^v'
      origin' = lookFrom

  return $ Camera {
    _viewportHeight=height,
    _viewportWidth=width,
    _focalLength=1,
    _origin=origin',
    _horizontal=horiz,
    _vertical=vert,
    _lowerLeftCorner=origin'-(horiz/2)-(vert/2)-focusDist*^w',
    _uu=u',
    _vv=v',
    _ww=w',
    _lensRadius=aperture/2
  }

makeRay :: Float -> Float -> Camera -> RandomState Ray
makeRay s t c = do
  uv <- getRandomInUnitDisk
  let (V3 x y _) = (c^.lensRadius)*^uv
      offset = (c^.uu)^*x + (c^.vv)^*y
      orig = (c^.origin) ^+^ offset
      dir = (c^.lowerLeftCorner) + s *^ (c^.horizontal) + t *^ (c^.vertical) - (c^.origin) - offset
  return $ Ray orig dir