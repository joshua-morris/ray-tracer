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

getRandomInUnitSquare :: RandomState Vec3
getRandomInUnitSquare = do
  gen <- get
  let (val, gen') = randomR (V3 0 0 0, V3 1 1 0) gen
  put gen'
  return val

getRandomInUnitSphere :: RandomState Vec3
getRandomInUnitSphere = do
  v <- getRandomInUnitCube
  if quadrance v < 1 then return v else getRandomInUnitSphere

getRandomInUnitDisk :: RandomState Vec3
getRandomInUnitDisk = do
  v <- getRandomInUnitSquare
  if quadrance v < 1 then return v else getRandomInUnitSquare

getUniformlyInRange :: Random a => (a, a) -> RandomState a
getUniformlyInRange (ub, lb) = do
  gen <- get
  let (val, gen') = randomR (ub, lb) gen
  put gen'
  return val