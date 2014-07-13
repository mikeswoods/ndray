{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Surface.Mapping (MappingShape(..), MappingEntity(..)
  ,defaultMappingShape, defaultMappingEntity
  ,planar, spherical, spherical2, cylindrical
  ,hitPosition, surfaceNormal, fromCentroid, reflection
  ,uv)
where
--import Raytracer.Utils.Math (invInterp)
--import Raytracer.World.Environment (worldUp, worldDown, worldLeft, worldRight)

--------------------------------------------------------------------------------

-- Types of texture mapping schemes
-- Adapted from notes at http://www.siggraph.org/education/materials/HyperGraph/mapping/r_wolfe/r_wolfe_mapping_2.htm
data MappingShape =
    Planar
  | Spherical Double
  | Cylindrical
  deriving (Show,Eq)


-- | Planar mapping constructor
planar :: MappingShape
planar = Planar

-- | Spherical mapping constructor with a set radius of 1.0
spherical :: MappingShape
spherical = Spherical 1.0

-- | Spherical mapping constructor, with an adjustable radius
spherical2 :: Double -> MappingShape
spherical2 = Spherical

-- | Cylindrical mapping constructor
cylindrical :: MappingShape
cylindrical = Cylindrical

-- | Default mapping shape constructor
defaultMappingShape :: MappingShape
defaultMappingShape = planar

--------------------------------------------------------------------------------

-- | Types entity mapping scheme
-- Adapted from notes at http://www.siggraph.org/education/materials/HyperGraph/mapping/r_wolfe/r_wolfe_mapping_4.htm
data MappingEntity =
    HitPosition
  | SurfaceNormal
  | FromCentroid
  | Reflection
  deriving (Show,Eq)


-- | Use the point of intersection for texture coordinate calculation
hitPosition :: MappingEntity
hitPosition = HitPosition

-- | Use the surface normal for texture coordinate calculation
surfaceNormal :: MappingEntity
surfaceNormal = SurfaceNormal

-- | Use the vector from the centroid of the object to the point of intersection
-- for the calculation
fromCentroid :: MappingEntity
fromCentroid = FromCentroid

-- | Use the surface reflection vector for the calculation
reflection :: MappingEntity
reflection = Reflection

-- | Default mapping entity constructor
defaultMappingEntity :: MappingEntity
defaultMappingEntity = hitPosition

--------------------------------------------------------------------------------

-- | Converts some <x,y,z> component in R^3 point in world space into a 2D
-- point (u,v) in texture space
uv :: MappingShape -> (Double,Double,Double) -> (Double,Double)
uv Planar        = uvPlanar
uv (Spherical r) = uvSpherical r
uv Cylindrical   = uvCylindrical


-- | Planar mapping
uvPlanar :: 
     (Double,Double,Double) -- ^ R^3 component to map to texture space
  -> (Double,Double)
uvPlanar (x,y,z) = (x*ux + y*uy + z*uz, x*vx + y*vy + z*vz)
  where
    (ux,uy,uz) = (y,-x,z)
    (vx,vy,vz) = (uy*vz - uz*vy,uz*vx - ux*vz,ux*vy - uy*vx)
{-# INLINE uvPlanar #-}


-- | Spherical mapping with adjustable radius
uvSpherical :: 
     Double                 -- ^ Sphere radius
  -> (Double,Double,Double) -- ^ R^3 component to map to texture space
  -> (Double,Double)
uvSpherical r (x,y,z) = (u,v)
  where
    pi2   = 2.0 * pi
    theta = atan2 z x
    phi   = asin $ y / r
    u     = if theta < 0.0 then (-theta) / pi2 else (1.0 - theta) / pi2
    v     = (phi / pi) + 0.5 
{-# INLINE uvSpherical #-}


-- | Cylindrical mapping
uvCylindrical :: 
     (Double,Double,Double) -- ^ R^3 component to map to texture space
  -> (Double,Double)
uvCylindrical (x,y,z) = (u,v)
  where
    theta = atan2 z x
    u     = theta / (2.0 * pi)
    v     = y + 0.5 
{-# INLINE uvCylindrical #-}


--uvCubic :: 
--     (PointR3,PointR3)      -- ^ The (minimum,maximum) vertices of the cube
--  -> (Double,Double,Double) -- ^ R^3 component to map to texture space
--  -> (Double,Double)
--uvCubic (v0,v1) p n
--  | n == worldUp || n == worldDown = 
--    (invInterp x xRange uvRange, invInterp z zRange uvRange)
--  | n == worldLeft || n == worldRight =
--    (invInterp y yRange uvRange, invInterp z zRange uvRange)
--  | otherwise =
--    (invInterp x xRange uvRange, invInterp y yRange uvRange)
--  where
--    (x,y,z) = (getX p,getY p,getZ p)
--    uvRange = (0.0, 1.0)
--    xRange  = (min (getX v0) (getX v1), max (getX v0) (getX v1))
--    yRange  = (min (getY v0) (getY v1), max (getY v0) (getY v1))
--    zRange  = (min (getZ v0) (getZ v1), max (getZ v0) (getZ v1))
--{-# INLINE uvCubic #-}