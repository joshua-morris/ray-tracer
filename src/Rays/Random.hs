module Rays.Random where

import Linear
import System.Random
import Control.Monad.State.Lazy

import Rays.Types

type RandomState a = State StdGen a

getRandomInUnitCube :: RandomState Vec3
getRandomInUnitCube = do
  gen <- get
  let (val, gen') = randomR (V3 0 0 0, V3 1 1 1) gen
  put gen'
  return val

getRandomInUnitSphere :: RandomState Vec3
getRandomInUnitSphere = do
  v <- getRandomInUnitCube
  if quadrance v < 1 then return v else getRandomInUnitSphere

getUniformlyInRange :: (Float, Float) -> RandomState Float
getUniformlyInRange (ub, lb) = do
  gen <- get
  let (val, gen') = randomR (ub, lb) gen
  put gen'
  return val