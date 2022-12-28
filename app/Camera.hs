module Camera where

import Vec3

data Camera = Camera 
    { viewportHeight :: Double
    , viewportWidth :: Double
    , focalLength :: Double 
    , origin :: Vec3 Double } deriving (Eq, Show)

horizontal :: Camera -> Vec3 Double
horizontal (Camera _ viewportWidth _ _) = Vec3 viewportWidth 0 0

vertical :: Camera -> Vec3 Double
vertical (Camera viewportHeight _ _ _) = Vec3 0 viewportHeight 0

lowerLeftCorner :: Camera -> Vec3 Double
lowerLeftCorner c@(Camera _ _ focalLength origin) = origin - (horizontal c)/2 - (vertical c)/2 - (Vec3 0 0 focalLength)