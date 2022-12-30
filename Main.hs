{-# LANGUAGE LambdaCase #-}

module Main where

import Rays.Types
import Rays.Ray
import Rays.Camera
import Rays.PPM
import Rays.Hittable
import Rays.Sphere
import Rays.Random
import Rays.Material

import Linear
import Control.Lens
import System.Random
import Control.Monad
import Control.Monad.State.Lazy

rayColour :: Hittable a => Ray -> [a] -> Int -> RandomState Vec3
rayColour ray@(Ray _ dir) hl depth = case hitAll hl ray 0.001 Nothing of
    Just h
      | depth >= 0 -> scatter (h^.material) ray h >>= \case
          Just (attenuation, r') -> (attenuation*) <$> rayColour r' hl (depth-1)
          _ -> return $ V3 0 0 0
      | otherwise -> return $ V3 0 0 0
    Nothing -> return $ lerp t_ (V3 1 1 1) (V3 0.5 0.7 1.0)
        where
            V3 _ y _ = normalize dir
            t_ = 0.5*(y + 1)

makeImage :: Hittable a => [a] -> Float -> Float -> Camera -> RandomState Image
makeImage hl height width camera =
  forM [[(i,j) | i <- [0..width-1]] | j <- [height-1,height-2..0]] $ \l -> do
    forM l $ \(x,y) -> do
      cols <- forM [1..25] $ \_ -> do
        r1 <- getUniformlyInRange (0,1)
        r2 <- getUniformlyInRange (0,1)
        let u = (x+r1)/(width-1)
            v = (y+r2)/(height-1)
            r = makeRay u v camera
        rayColour r hl 50
      return $ (sum cols) / 25

main :: IO ()
main = do
  let aspectRatio = 3.0/2.0
  let imageWidth = 400
  let imageHeight = imageWidth / aspectRatio
  
  let camera = Camera {
      _viewportHeight=2.0,
      _viewportWidth=aspectRatio*2.0,
      _focalLength=1.0,
      _origin=(V3 0 0 0)
  }

  let sphere1 = Sphere (V3 0 0 (-1)) 0.5 (Matte (V3 0.7 0.3 0.3))
  let sphere2 = Sphere (V3 0 (-100.5) (-1)) 100 (Matte (V3 0.8 0.8 0.0))
  let sphere3 = Sphere (V3 (-1) 0 (-1)) (-0.4) (Glass 1.5)
  let sphere4 = Sphere (V3 1 0 (-1)) 0.5 (Metal (V3 0.8 0.6 0.2) 0.1)

  let world = [sphere1, sphere2, sphere3, sphere4]

  gen <- initStdGen
  let image = evalState (makeImage world imageHeight imageWidth camera) gen
  savePPM "image.ppm" image
