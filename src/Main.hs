module Main where

import Types
import Ray
import Camera
import PPM
import Hittable
import Sphere
import Linear

import Control.Lens
import System.Random
import Data.List

rayColour :: Hittable a => Ray -> [a] -> Vec3
rayColour ray@(Ray orig dir) hl = case hitAll hl ray 0 Nothing of
    Just h -> 0.5 *^ (h^.normal + (V3 1 1 1))
    Nothing -> lerp t (V3 1 1 1) (V3 0.5 0.7 1.0)
        where
            V3 _ y _ = normalize dir
            t = 0.5*(y + 1)

generateN :: RandomGen g => Int -> g -> [Float]
generateN n = take n . unfoldr (Just . uniformR (0, 1))

antialias :: Hittable a => Int -> [a] -> Float -> Float -> Float -> Float -> Camera -> Vec3
antialias n hl i j height width c = 
    (1/(fromIntegral n)) *^ (sum $ [rayColour (makeRay ((i + r)/(width-1)) ((j + r)/(height-1)) c) hl | r <- generateN n pureGen])
        where
            pureGen = mkStdGen 42

makeImage :: Hittable a => [a] -> Float -> Float -> Camera -> Image
makeImage hl height width camera = 
    [[antialias 100 hl i j height width camera | i <- [0..width-1]] | j <- [height-1,height-2..0]]

main :: IO ()
main = do
    let aspectRatio = 16.0/9.0
    let imageWidth = 600
    let imageHeight = imageWidth / aspectRatio
    
    let camera = Camera {
        _viewportHeight=2.0,
        _viewportWidth=aspectRatio*2.0,
        _focalLength=1.0,
        _origin=(V3 0 0 0)
    }

    let sphere1 = Sphere (V3 0 0 (-1)) 0.5
    let sphere2 = Sphere (V3 0 (-100.5) (-1)) 100

    let world = [sphere1, sphere2]

    savePPM "image.ppm" (makeImage world imageHeight imageWidth camera)
