{-# LANGUAGE TemplateHaskell #-}

module Rays.Camera where

import Control.Lens

import Rays.Util
import Rays.Types
import Linear
import Rays.Ray

data Camera = Camera 
  { _viewportHeight :: !Float
  , _viewportWidth :: !Float
  , _focalLength :: !Float 
  , _origin :: !Vec3
  , _horizontal :: !Vec3
  , _vertical :: !Vec3
  , _lowerLeftCorner :: !Vec3
  } deriving (Eq,Show)

makeLenses ''Camera

cameraBuilder :: Float -> Float -> Vec3 -> Vec3 -> Vec3 -> Camera
cameraBuilder vfov aspectRatio lookFrom lookAt vup =
  let theta  = degreesToRadians vfov
      h      = tan (theta/2)
      height = 2*h
      width  = aspectRatio*height
      w      = normalize (lookFrom - lookAt)
      u      = normalize (cross vup w)
      v      = cross w u
      horiz  = width*^u
      vert   = height*^v
      origin' = lookFrom

  in Camera {
    _viewportHeight=height,
    _viewportWidth=width,
    _focalLength=1,
    _origin=origin',
    _horizontal=horiz,
    _vertical=vert,
    _lowerLeftCorner=origin'-(horiz/2)-(vert/2)-w
  }

makeRay :: Float -> Float -> Camera -> Ray
makeRay u v c = Ray (c^.origin) ((c^.lowerLeftCorner) + u *^ (c^.horizontal) + v *^ (c^.vertical) - (c^.origin)) 