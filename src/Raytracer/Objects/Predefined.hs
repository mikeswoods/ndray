{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

module Raytracer.Objects.Predefined
where
import Control.Lens ((^.))
import Raytracer.Objects.Object
import Raytracer.World.Const
import qualified Raytracer.Parsers.WaveFront as WF
import Raytracer.Objects.Attributes (Attributes)
import Raytracer.Space.Tensor 
  (
   vec3
  ,point3
  ,cross
  ,unit
  ,(/=>)
  )
import Raytracer.Objects.Primitive (Primitive(..))

--------------------------------------------------------------------------------

sphere ::
     Int         -- ^ Object sequence ID
  -> Double      -- ^ The radius of the sphere
  -> Attributes  -- ^ The attributes of the box
  -> WorldObject
sphere seqId radius attrs =
  createObject seqId "Sphere" [(Sphere radius)] attrs


plane ::
     Int        -- ^ Object sequence ID
  -> R3Coord    -- ^ The (x,y,z) normal of the plane
  -> Double     -- ^ The distance from the origin of the plane
  -> Attributes -- ^ The attributes of the plane
  -> WorldObject
plane seqId (nx,ny,nz) dist attrs = 
  createObject seqId "Plane" [obj] attrs
  where
    n     = unit $ vec3 nx ny nz
    uAxis = vec3 ny (-nx) nz
    vAxis = n `cross` uAxis
    obj   = Plane (n,dist) (uAxis,vAxis)


triangle ::
     Int        -- ^ Object sequence ID
  -> R3Coord    -- ^ The (x,y,z) coordinate of a vertex of the triangle
  -> R3Coord    -- ^ The (x,y,z) coordinate of a vertex of the triangle
  -> R3Coord    -- ^ The (x,y,z) coordinate of a vertex of the triangle
  -> Attributes -- ^ The attributes of the box
  -> WorldObject
triangle seqId (xv0,yv0,zv0) (xv1,yv1,zv1) (xv2,yv2,zv2) attrs = 
  createObject seqId "Triangle" [obj] attrs
  where
    obj        = Triangle (v0,v1,v2) (e1,e2) (n,n,n) -- use uniform normal
    (v0,v1,v2) = (point3 xv0 yv0 zv0,point3 xv1 yv1 zv1,point3 xv2 yv2 zv2)
    (e1,e2)    = (v1 /=> v0,v2 /=> v0)
    n          = e1 `cross` e2


box ::
     Int        -- ^ Object sequence ID
  -> R3Coord    -- ^ The (x,y,z) coordinate of the box's minimum corner
  -> R3Coord    -- ^ The (x,y,z) coordinate of the box's minimum corner
  -> Attributes -- ^ The attributes of the box
  -> WorldObject
box seqId (xMin, yMin, zMin) (xMax, yMax, zMax) attrs = 
  createObject seqId "Box" [obj] attrs
  where
    obj = Box (point3 xMin yMin zMin, point3 xMax yMax zMax)


-- | Generates a triangle mesh object loaded from the specified file. If the
-- model file does not exist or cannot be parsed, an error will be raised
--
-- Note: Model files are loaded relative to the "<Project>/Models" directory
--
mesh ::
     Int        -- ^ Object sequence ID
  -> FilePath   -- ^ The model file to load the mesh data from
  -> Bool       -- ^ Flag to auto-scale the mesh such that all coordinates
                -- fall within [0.0,1.0]
  -> Attributes -- ^ Attributes to apply the the loaded mesh data
  -> IO WorldObject
mesh seqId modelFilePath autoScale attrs = do 
  meshData <- WF.parseFile modelFilePath autoScale
  return $ createObject seqId "Mesh" (primitives meshData) attrs
  where
    mkTriangle t = Triangle vertices edges normals
      where
        vertices@(v0,v1,v2) = t^.WF.vertices
        edges               = (v1 /=> v0,v2 /=> v0)
        normals             = t^.WF.normals
    primitives meshData = fmap mkTriangle $ WF.triangles meshData

