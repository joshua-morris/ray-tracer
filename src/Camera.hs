{-# LANGUAGE TemplateHaskell #-}

module Camera where

import Control.Lens

import Types
import Linear
import Ray

data Camera = Camera 
  { _viewportHeight :: !Float
  , _viewportWidth :: !Float
  , _focalLength :: !Float 
  , _origin :: !Vec3 
  } deriving (Eq,Show)

makeLenses ''Camera

horizontal :: Camera -> Vec3
horizontal (Camera _ w _ _) = V3 w 0 0

vertical :: Camera -> Vec3
vertical (Camera h _ _ _) = V3 0 h 0

lowerLeftCorner :: Camera -> Vec3
lowerLeftCorner c@(Camera _ _ l o) = o - (horizontal c)/2 - (vertical c)/2 - (V3 0 0 l)

makeRay :: Float -> Float -> Camera -> Ray
makeRay u v c = Ray (c^.origin) ((lowerLeftCorner c) + u *^ (horizontal c) + v *^ (vertical c) - (c^.origin)) 