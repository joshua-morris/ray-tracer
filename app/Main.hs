module Main where

import Vec3
import Ray
import Camera
import PPM
import Hittable
import Sphere

makeRay :: Double -> Double -> Double -> Double -> Camera -> Ray
makeRay i j height width c = 
    Ray (origin c) ((lowerLeftCorner c) + scale u (horizontal c) + scale v (vertical c) - (origin c)) 
    where
        u = i / (width-1)
        v = j / (height-1)

rayColour :: Hittable a => Ray -> [a] -> Vec3 Double
rayColour ray hl = case hitAll hl ray 0 Nothing of
    Just h -> scale 0.5 ((normal h) + (Vec3 1 1 1))
    Nothing -> lerp t (Vec3 1 1 1) (Vec3 0.5 0.7 1.0)
        where
            unitRay = vnormalise (dir ray)
            t = 0.5*(vy unitRay + 1)

makeImage :: Hittable a => [a] -> Double -> Double -> Camera -> Image
makeImage hl height width camera = 
    [[rayColour (makeRay i j height width camera) hl | i <- [0..width-1]] | j <- [height-1,height-2..0]]

main :: IO ()
main = do
    let aspectRatio = 16.0/9.0
    let imageWidth = 400
    let imageHeight = imageWidth / aspectRatio
    
    let camera = Camera {
        viewportHeight=2.0,
        viewportWidth=aspectRatio*2.0,
        focalLength=1.0,
        origin=(Vec3 0 0 0)
    }

    let sphere1 = Sphere {
        center=(Vec3 0 0 (-1)),
        radius=0.5
    }
    let sphere2 = Sphere {
        center=(Vec3 0 (-100.5) (-1)),
        radius=100
    }

    let world = [sphere1, sphere2]

    savePPM "image.ppm" (makeImage world imageHeight imageWidth camera)
