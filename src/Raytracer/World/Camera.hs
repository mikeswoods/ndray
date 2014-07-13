{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.World.Camera 
  (
   Camera
  ,defaultCamera
  ,camera
  ,lookAt
  ,position
  ,fov
  ,up
  ,right
  ,forward
  )
where
import Data.Typeable 
  (
   Typeable
  )
import Text.Printf 
  (
   printf
  )
import Control.Lens 
  (
   (^.)
  ,makeLenses
  )
import Raytracer.World.Const 
  (
   R3Coord
  )
import Raytracer.Space.Tensor 
  (
   VectorR3
  ,PointR3
  ,AffineTransformable
  ,vec3
  ,point3
  ,(<!-)
  ,(<!-)
  ,(/->)
  ,(|->)
  ,cross)

--------------------------------------------------------------------------------

data Camera = Camera
  { _position :: PointR3    -- ^ The position of the camera the world
  , _lookAt   :: PointR3    -- ^ Point in R^3 the camera "looks" at
  , _fov      :: Double     -- ^ The camera field-of-view, default ~ 60
  , _up       :: VectorR3   -- ^ The vector defining the "up" direction
  , _right    :: VectorR3   -- ^ the vector defining the "right" direction
  , _forward  :: VectorR3}  -- ^ The vector defining the "forward" direction
  deriving (Eq,Typeable)

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

--------------------------------------------------------------------------------

-- | World "up" vector
upVector :: VectorR3
upVector = vec3 0 1 0


-- | Constructs a new Camera instance
camera ::    R3Coord -- ^ the point where the camera is positioned
          -> R3Coord -- ^ the point the camera is looking at in virtual space 
          -> Double  -- ^ Field of view (FOV) value
          -> Camera
camera (px,py,pz) (lx,ly,lz) fov' = 
  Camera { _position = cPosition
         , _lookAt   = cLookAt
         , _fov      = fov'
         , _up       = cUp
         , _right    = cRight
         , _forward  = cForward }
  where
    cPosition = (point3 px py pz)
    cLookAt   = point3 lx ly lz
    cForward  = cPosition /-> cLookAt
    --cRight    = cForward `cross` up
    --cUp       = cRight `cross` cForward
    cRight    = upVector `cross` cForward 
    cUp       = cForward `cross` cRight


-- | The default camera definition
defaultCamera :: Camera
defaultCamera = camera (0, 0.5, -4.0) (0.0, 0.5, 0.0) 60
