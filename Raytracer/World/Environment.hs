{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Raytracer.World.Environment (Coordinate
  ,worldUp, worldDown, worldRight, worldLeft, worldForward, worldBackward
  ,worldOrigin, correction, maxDist
  ,camera, Camera(..), lookAt, position, fov, up, right, forward)
where
import Text.Printf (printf)
import Control.Lens ((^.), makeLenses)
import Raytracer.Utils.Math (epsilon)
import Raytracer.Space.Tensor (VectorR3, vec3, PointR3, point3
  ,AffineTransformable, (<!-), (<!-), (/->), (|->), cross)

--------------------------------------------------------------------------------

-- | Simple type synonym for a generic (x,y,z) tuple with components in R
type Coordinate = (Double,Double,Double)


-- | "Up" orientation vector
worldUp :: VectorR3
worldUp = vec3 0.0 1.0 0.0

-- | "Down" orientation vector
worldDown :: VectorR3
worldDown = vec3 0.0 (-1.0) 0.0

-- | "right" orientation vector
worldRight :: VectorR3
worldRight = vec3 1.0 0.0 0.0

-- | "left" orientation vector
worldLeft :: VectorR3
worldLeft = vec3 (-1.0) 0.0 0.0

-- | "Forward" orientation vector
worldForward :: VectorR3
worldForward = vec3 (-1.0) 0.0 (-1.0)

-- | "Backward" orientation vector
worldBackward :: VectorR3
worldBackward = vec3 0.0 0.0 1.0

-- | World "origin" point
worldOrigin :: PointR3
worldOrigin = point3 0.0 0.0 0.0

-- | "Correction" vector
correction :: VectorR3
correction = vec3 epsilon epsilon epsilon

-- | The world maximum distance
maxDist :: Double
maxDist = 1.0e5

--------------------------------------------------------------------------------

data Camera = Camera
  { _position :: PointR3    -- ^ The position of the camera the world
  , _lookAt   :: PointR3    -- ^ Point in R^3 the camera "looks" at
  , _fov      :: Double     -- ^ The camera field-of-view, default ~ 60
  , _up       :: VectorR3   -- ^ The vector defining the "up" direction
  , _right    :: VectorR3   -- ^ the vector defining the "right" direction
  , _forward  :: VectorR3}  -- ^ The vector defining the "forward" direction

makeLenses ''Camera


instance Show Camera where

  show cam = printf
       ("Camera {\n"           ++
        "  position = %s,\n"   ++
        "  lookAt   = %s,\n"   ++
        "  fov      = %.2f,\n" ++
        "  up       = %s,\n"   ++
        "  right    = %s,\n"   ++
        "  forward  = %s\n"    ++
        "}\n")
       (show $ cam^.position)
       (show $ cam^.lookAt)
       (cam^.fov)
       (show $ cam^.up)
       (show $ cam^.right)
       (show $ cam^.forward)


-- | Constructs a new Camera instance
camera ::    Coordinate -- ^ the point where the camera is positioned
          -> Coordinate -- ^ the point the camera is looking at in virtual space 
          -> Double     -- ^ Field of view (FOV) value
          -> Camera
camera (px,py,pz) (lx,ly,lz) fov' = 
  Camera { _position = cPosition
         , _lookAt   = cLookAt
         , _fov      = fov'
         , _up       = cUp
         , _right    = cRight
         , _forward  = cForward }
  where
    cPosition = (point3 px py pz) |-> correction
    cLookAt   = point3 lx ly lz
    cForward  = cPosition /-> cLookAt
    --cRight    = cForward `cross` worldUp
    --cUp       = cRight `cross` cForward
    cRight    = worldUp `cross` cForward 
    cUp       = cForward `cross` cRight


-- | Camera <: AffineTransformable
instance AffineTransformable Camera where
  cam <!- m = 
    Camera { _position = cPosition'
           , _lookAt   = cLookAt'
           , _fov      = cam^.fov
           , _up       = cUp'
           , _right    = cRight'
           , _forward  = cForward' }
    where
      cPosition' = (cam^.position) <!- m
      cForward'  = (cam^.forward)  <!- m
      cRight'    = (cam^.right)    <!- m
      cUp'       = (cam^.up)       <!- m
      cLookAt'   = cPosition' |-> cForward'
