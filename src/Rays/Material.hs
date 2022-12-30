module Rays.Material where

import Rays.Types
import Rays.Ray

import Control.Lens

data Material = 
  Matte !Vec3
  | Metal !Vec3 !Float
  | Glass !Float
  deriving (Show,Eq)