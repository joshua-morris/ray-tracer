module Camera where

import Types
import Linear
import Ray

data Camera = Camera 
    { viewportHeight :: Float
    , viewportWidth :: Float
    , focalLength :: Float 
    , origin :: Vec3 } deriving (Eq, Show)

horizontal :: Camera -> Vec3
horizontal (Camera _ viewportWidth _ _) = V3 viewportWidth 0 0

vertical :: Camera -> Vec3
vertical (Camera viewportHeight _ _ _) = V3 0 viewportHeight 0

lowerLeftCorner :: Camera -> Vec3
lowerLeftCorner c@(Camera _ _ focalLength origin) = origin - (horizontal c)/2 - (vertical c)/2 - (V3 0 0 focalLength)

makeRay :: Float -> Float -> Camera -> Ray
makeRay u v c = Ray (origin c) ((lowerLeftCorner c) + u *^ (horizontal c) + v *^ (vertical c) - (origin c)) 