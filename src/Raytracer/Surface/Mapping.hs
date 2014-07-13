{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Surface.Mapping (
   EntityType(..)
  ,Mapping(..)
  ,usingPosition
  ,usingNormal
  ,usingCentroid
  ,usingReflection
  ,mapToX
  ,mapToY
  ,mapToZ
  ,mapToSphere
  ,mapToCylinder
  ,mapToCube
  ,findUV
  ,entityType
  ,(.!!.)
  ,defaultMapping
)
where
import Control.Exception (
    throw
  )
import Control.Lens 
  (
   Lens'
  ,lens
  )
import Raytracer.World.Const 
  (
   R3Coord
  )
import Data.Typeable 
  (
   Typeable
  )
import Raytracer.World.AABB 
  (
   AABB
  )
import Raytracer.Surface.Texture 
  (
   Texture(..)
  ,UVCoord(..)
  ,TextureError(..)
  )
import Raytracer.Surface.Texture.Basic 
  (
   defaultTexture
  )
import Raytracer.Space.Tensor (
   R3(..)
  )

--------------------------------------------------------------------------------

-- | Determines which "entity" (x,y,z) triple will be used for computed texel 
-- value. A mapping entity can be an (Double,Double,Double) value, such as a 
-- position, vector, etc.
data EntityType = 
    Position   -- ^ Use the (x,y,z) position of the point of intersection in object (not world) space
  | Normal     -- ^ Use the (x,y,z) position of computed normal at the point of intersection
  | Centroid   -- ^ Use the (x,y,z) position computed normal from the centroid of the object to the point of intersection
  | Reflection -- ^ Use the (x,y,z) position of the average of all reflection rays from the object at the point of intersection
  deriving (Show,Eq,Typeable)

--------------------------------------------------------------------------------

usingPosition :: EntityType
usingPosition = Position

usingNormal :: EntityType
usingNormal = Normal

usingCentroid :: EntityType
usingCentroid = Centroid

usingReflection :: EntityType
usingReflection = Reflection

--------------------------------------------------------------------------------

-- | Determines the shape of of the texture mapping scheme employed when
-- computing a texel value
data Mapping =
    XPlanarMap Texture EntityType     -- ^ Map the texture along the X-axis of the entity (x,y,z) triple
  | YPlanarMap Texture EntityType     -- ^ Map the texture along the Y-axis of the entity (x,y,z) triple
  | ZPlanarMap Texture EntityType     -- ^ Map the texture along the Z-axis of the entity (x,y,z) triple
  | SphericalMap Texture EntityType   -- ^ Map along a sphere enclosing the object
  | CylindricalMap Texture EntityType -- ^ Map along a cylinder enclosing the object
  | CubicMap [Texture] EntityType     -- ^ Map along a cube enclosing the object
  deriving (Show, Typeable)

--------------------------------------------------------------------------------

-- | Default mapping texture: Map along the X-plane, using the position of
-- the point of intersection
defaultMapping :: Mapping
defaultMapping = XPlanarMap defaultTexture Position

--------------------------------------------------------------------------------

-- | Lens for the mapping's entity type
entityType :: Lens' Mapping EntityType
entityType = lens getter setter
  where
    getter (XPlanarMap _ e)     = e
    getter (YPlanarMap _ e)     = e
    getter (ZPlanarMap _ e)     = e
    getter (SphericalMap _ e)   = e
    getter (CylindricalMap _ e) = e
    getter (CubicMap _ e)       = e

    setter (XPlanarMap t _) e     = XPlanarMap t e
    setter (YPlanarMap t _) e     = YPlanarMap t e
    setter (ZPlanarMap t _) e     = ZPlanarMap t e
    setter (SphericalMap t _) e   = SphericalMap t e
    setter (CylindricalMap t _) e = CylindricalMap t e
    setter (CubicMap t _) e       = CubicMap t e

--------------------------------------------------------------------------------

mapToX :: Texture -> EntityType -> Mapping
mapToX = XPlanarMap

mapToY :: Texture -> EntityType -> Mapping
mapToY = YPlanarMap

mapToZ :: Texture -> EntityType -> Mapping
mapToZ = ZPlanarMap

mapToSphere :: Texture -> EntityType -> Mapping
mapToSphere = SphericalMap

mapToCylinder :: Texture -> EntityType -> Mapping
mapToCylinder = CylindricalMap

mapToCube :: [Texture] -> EntityType -> Mapping
mapToCube = CubicMap

--------------------------------------------------------------------------------

-- Given a face index, this function will return the corresponding texture
(.!!.)  :: 
     Mapping -- ^ The mapping to select the tetxure from
  -> Int     -- ^ The index of the texture to select
  -> Texture
(.!!.) (XPlanarMap t _) _     = t
(.!!.) (YPlanarMap t _) _     = t
(.!!.) (ZPlanarMap t _) _     = t
(.!!.) (SphericalMap t _) _   = t
(.!!.) (CylindricalMap t _) _ = t
(.!!.) (CubicMap ts _) ix 
  | ix < 0           = throw $ FaceIndexOutOfBounds ix
  | ix > (length ts) = throw $ FaceIndexOutOfBounds ix
  | otherwise        = ts !! (ix - 1)


-- | Converts an R3 point (x,y,z) in world space into a 2D point (u,v) in 
-- texture space 
findUV :: 
     Mapping -- ^ The mapping to use
  -> AABB    -- ^ The axis-aligned bounding bound to use to compute the
             -- ^ minimum and maximum extent of the enclosing mapping shape 
  -> R3Coord -- ^ The (x,y,z) value to map
  -> UVCoord
findUV (XPlanarMap _ _) _ (_,y,z) = UVCoord (y,z) 1

findUV (YPlanarMap _ _) _ (x,_,z) = UVCoord (x,z) 1

findUV (ZPlanarMap _ _) _ (x,y,_) = UVCoord (x,y) 1

-- trace (printf "u=%.4f ; v=%.4f" u v)
findUV (SphericalMap _ _) (v0,v1) (x,y,z) = UVCoord (u,v) 1
  where
    x'  = abs $ ((getX v0) - (getX v1)) / 2.0
    y'  = abs $ ((getY v0) - (getY v1)) / 2.0
    z'  = abs $ ((getZ v0) - (getZ v1)) / 2.0
    r   = min x' $ min y' z'
    u   = 0.5 + ((atan2 z x) / pi)
    v   = 0.5 - ((asin (y / r)) / (pi * 0.5))

findUV (CylindricalMap _ _) _ (x,y,z) = UVCoord (u,v) 1
  where
    theta = atan2 z x
    u     = theta / (pi * 0.5)
    v     = y + 0.5 

findUV (CubicMap _ _) _ (x,y,z) 
  | absX == maxC = 
    let zx2 = z / (2 * x)
        yx2 = y / (2 * x)
    in  if x > 0
          then UVCoord (-zx2, 0.5 - yx2) 2
          else UVCoord (-zx2, 0.5 + yx2) 1
  | absY == maxC = 
    let xy2 = x / (2 * y)
        zy2 = z / (2 * y)
    in  if y > 0
          then UVCoord (-xy2, 0.5 - zy2) 3 
          else UVCoord (xy2, 0.5 - zy2) 4
  | otherwise = 
    let xz2 = x / (2 * z)
        yz2 = y / (2 * z)
    in  if z > 0
          then UVCoord (xz2, 0.5 - yz2) 5
          else UVCoord (-xz2, 0.5 + yz2) 6
  where
    (absX,absY,absZ) = (abs x,abs y,abs z)
    maxC             = max absX $ max absY absZ
