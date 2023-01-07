{-# LANGUAGE LambdaCase #-}

module Main where

import Rays.Types
import Rays.Ray
import Rays.Camera
import Rays.Hittable
import Rays.Sphere
import Rays.Random
import Rays.Material

import Linear
import Control.Lens
import System.Random
import Control.Monad
import Control.Monad.State.Lazy
import Codec.Picture

aspectRatio :: Float
aspectRatio = 3.0/2.0

width :: Int
width = 400

height :: Int
height = round $ (fromIntegral width)/aspectRatio

samples :: Int
samples = 200

channel :: Float -> Int
channel = floor . (255*) . min 1 . max 0

toPixel :: Vec3 -> PixelRGB8
toPixel v = PixelRGB8 r g b
  where
    V3 r g b = (fromIntegral . channel) <$> v

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

generator :: Hittable a => [a] -> Camera -> Int -> Int -> RandomState PixelRGB8
generator hl camera x y = do
  cols <- forM [1..samples] $ \_ -> do
    r1 <- getUniformlyInRange (0,1)
    r2 <- getUniformlyInRange (0,1)
    let u = (fromIntegral x+r1)/(fromIntegral width-1)
        v = (fromIntegral y+r2)/(fromIntegral height-1)
    r <- makeRay u v camera
    rayColour r hl 50
  return $ toPixel $ (sum cols) / (fromIntegral samples)

randomScene :: RandomState [Sphere]
randomScene = do
  balls <- forM [(a, b) | a <- [-11..10], b <- [-11..10]] $ \(a,b) -> do
    chooseMat <- getUniformlyInRange (0::Float, 1)
    r1 <- getUniformlyInRange (0::Float, 1)
    r2 <- getUniformlyInRange (0::Float, 1)
    randomFuzz <- getUniformlyInRange (0,0.5)
    randomColour1 <- getUniformlyInRange (V3 0 0 0, V3 1 1 1)
    randomColour2 <- getUniformlyInRange (V3 0 0 0, V3 0.5 0.5 0.5)

    let center = V3 (a+0.9*r1) 0.2 (b+0.9*r2)
        material = case (chooseMat < 0.8, chooseMat < 0.95) of
          (True,_) -> Matte (randomColour1*randomColour1)
          (_,True) -> Metal randomColour2 randomFuzz
          otherwise -> Glass 1.5
    
    return $ Sphere center 0.2 material


  let groundMaterial = Matte (V3 0.5 0.5 0.5)
      material1      = Glass 1.5
      material2      = Matte (V3 0.4 0.2 0.1)
      material3      = Metal (V3 0.7 0.6 0.5) 0

      ground         = Sphere (V3 0 (-1000) 0) 1000 groundMaterial
      sphere1        = Sphere (V3 0 1 0) 1 material1
      sphere2        = Sphere (V3 (-4) 1 0) 1 material2
      sphere3        = Sphere (V3 4 1 0) 1 material3
  
  return $ ([ground, sphere1, sphere2, sphere3] ++ balls)


main :: IO ()
main = do
  let lookFrom = (V3 13 2 3)
      lookAt = (V3 0 0 0)
      focusDist = 10.0
      aperture = 0.1

  gen <- initStdGen
  let (camera, gen2) = runState (cameraBuilder 20 aspectRatio lookFrom lookAt (V3 0 1 0) aperture focusDist) gen
  let (world, gen3) = runState randomScene gen2
  let image = generateImage (\x y -> evalState (generator world camera x (height-y)) gen3) width height
  writePng "image.png" image
