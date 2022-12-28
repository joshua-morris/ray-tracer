module Main where

import Vec3
import Ray
import Camera
import PPM

makeRay :: Double -> Double -> Double -> Double -> Camera -> Ray
makeRay i j height width c = 
    Ray (origin c) ((lowerLeftCorner c) + vscale u (horizontal c) + vscale v (vertical c) - (origin c)) 
    where
        u = i / (width-1)
        v = j / (height-1)

rayColour :: Ray -> Vec3 Double
rayColour r@(Ray orig dir) = case hitSphere r (Vec3 0 0 (-1)) 0.5 of
    Just x -> 0.5*(Vec3 (x+1) (y+1) (z+1))
        where
            (Vec3 x y z) = vnormalise (rayAt r t - (Vec3 0 0 (-1)))
            unitRay = vnormalise dir
            t = 0.5*(vy unitRay) + 0.5
    Nothing -> lerp t (Vec3 1 1 1) (Vec3 0.5 0.7 1.0)
        where
            unitRay = vnormalise dir
            t = 0.5*(vy unitRay) + 0.5

makeImage :: Double -> Double -> Camera -> Image
makeImage height width camera = 
    [[rayColour $ makeRay i j height width camera | i <- [0..width-1]] | j <- [height-1,height-2..0]]

main :: IO ()
main = do
    let aspectRatio = 16.0/9.0
    let imageWidth = 400
    let imageHeight = imageWidth / aspectRatio
    
    let camera = Camera {
        viewportHeight=2.0,
        viewportWidth=aspectRatio*2.0,
        focalLength=1.0,
        origin=vzero
    }
    savePPM "image.ppm" (makeImage imageHeight imageWidth camera)
