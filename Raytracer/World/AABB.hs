{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.World.AABB (AABB, WithAABB(..), newAABB
  ,vMin, vMax, hits)
where
import Data.Tuple (swap)
import Control.Lens (Lens', lens, (^.))
import Data.Monoid
import Raytracer.World.Environment (worldOrigin)
import Raytracer.Space.Tensor (R3(..), VectorR3, PointR3, point3, vec3, (|->)
  ,AffineMatrix, AffineTransformable(..))
import Raytracer.Utils.Math (epsilon)

--------------------------------------------------------------------------------

-- | Simple axis-aligned bounding box definition -> Defined by two points,
-- v0 and v0, the minimum and maximum extent of the box (bottom-left-forward 
-- corner to the top-right-back corner)
type AABB = (PointR3, PointR3)


-- | Gets/sets the minimum point of the AABB
vMin :: Lens' AABB PointR3
vMin = lens getter setter
  where
    getter (p0,_)     = p0
    setter (_,p1) p0' = (p0',p1)


-- | Gets/sets the maximum point of the AABB
vMax :: Lens' AABB PointR3
vMax = lens getter setter
  where
    getter ((_,p1))     = p1
    setter ((p0,_)) p1' = (p0,p1')


-- | Creates a new AABB
newAABB ::    PointR3 -- ^ Minimum point
           -> PointR3 -- ^ Maximum point
           -> AABB
newAABB p1 p2 = (p1,p2)

--------------------------------------------------------------------------------

-- | Tests if the given ray hits the AABB defined by the given extents
hits ::    PointR3  -- ^ The origin of the incoming ray to test
        -> VectorR3 -- ^ The direction of the incoming ray to test
        -> AABB     -- ^ The AABB (min,max) extents
        -> Bool
hits o d bbox
  | extentIn > extentOut || extentOut < 0 = False
  | otherwise = True
  where
    checkAndSwap v
      | v > 0     = id 
      | otherwise = swap
    (v0,v1)    = (bbox^.vMin,bbox^.vMax)
    (ox,oy,oz) = (getX o,getY o,getZ o)
    (dx,dy,dz) = (getX d,getY d,getZ d)
    (x,x')     = checkAndSwap dx ((getX v0 - ox) / getX d, (getX v1 - ox) / dx)
    (y,y')     = checkAndSwap dy ((getY v0 - oy) / dy, (getY v1 - oy) / dy)
    (z,z')     = checkAndSwap dz ((getZ v0 - oz) / dz, (getZ v1 - oz) / dz)
    extentIn   = max x $ max y z
    extentOut  = min x' $ min y' z'
{-# INLINE hits #-}

--------------------------------------------------------------------------------

instance AffineTransformable AABB where

  (<!-) = transformAccurate
  {-# INLINE (<!-) #-}

-- | More accurate, but slower AABB transformer
transformAccurate ::    AABB 
                     -> AffineMatrix 
                     -> AABB
transformAccurate bbox m = 
  newAABB (point3 minX minY minZ) (point3 maxX maxY maxZ)
  where
    (v0,v1)       = (bbox^.vMin,bbox^.vMax)
    (v0x,v0y,v0z) = (getX v0,getY v0,getZ v0)
    (v1x,v1y,v1z) = (getX v1,getY v1,getZ v1)
    -- http://www.cs.unc.edu/~zhangh/technotes/bbox.pdf
    c1 = (point3 v0x v0y v0z) <!- m
    c2 = (point3 v0x v1y v0z) <!- m
    c3 = (point3 v0x v1y v1z) <!- m
    c4 = (point3 v0x v0y v1z) <!- m
    c5 = (point3 v1x v0y v0z) <!- m
    c6 = (point3 v1x v1y v0z) <!- m
    c7 = (point3 v1x v1y v1z) <!- m
    c8 = (point3 v1x v0y v1z) <!- m
    xs = [getX c1, getX c2, getX c3, getX c4, getX c5, getX c6, getX c7,getX c8]
    ys = [getY c1, getY c2, getY c3, getY c4, getY c5, getY c6, getY c7,getY c8]
    zs = [getZ c1, getZ c2, getZ c3, getZ c4, getZ c5, getZ c6, getZ c7,getZ c8]
    (minX,maxX) = (minimum xs,maximum xs)
    (minY,maxY) = (minimum ys,maximum ys)
    (minZ,maxZ) = (minimum zs,maximum zs)


-- | Adapted from 
-- http://zeuxcg.org/2010/10/17/aabb-from-obb-with-component-wise-abs/
transformFast ::    AABB 
                 -> AffineMatrix 
                 -> AABB
transformFast bbox m = newAABB (center' |-> (-extent'')) (center' |-> extent')
  where
    (v0,v1)       = (bbox^.vMin,bbox^.vMax)
    (v0x,v0y,v0z) = (getX v0,getY v0,getZ v0)
    (v1x,v1y,v1z) = (getX v1,getY v1,getZ v1)
    center   = point3 (v1x + v0x / 2.0) (v1y + v0y / 2.0) (v1z + v0z / 2.0)
    extent'  = vec3 (v1x - v0x / 2.0) (v1y - v0y / 2.0) (v1z - v0z / 2.0)
    center'  = center <!- m
    extent'' = extent' <!- abs m

--------------------------------------------------------------------------------

-- | Bounding box intersection test
class WithAABB a where

  -- | Returns the (minimum,maximum) points representing the extent of the
  -- object
  extent ::    a  -- ^ The object to compute the extent of
            -> (PointR3, PointR3)


  -- | Returns the centroid of the object.
  -- This is the default implementation. By default the bounding box of the
  -- object is calculated and the centroid is calculated as
  --
  --   ((px + px') / 2.0, (py + py') / 2.0, (pz + pz') / 2.0) 
  --
  centroid ::    a       -- ^ The object to find the centroid of
              -> PointR3
  centroid obj = point3 cx cy cz
    where
      (p,p')        = extent obj
      (px,py,pz)    = (getX p,getY p,getZ p)
      (px',py',pz') = (getX p',getY p',getZ p')
      (cx,cy,cz)    = ((px + px') / 2.0, (py + py') / 2.0, (pz + pz') / 2.0) 


  -- | Finds the centroid of a list of objects
  findCentroid ::     [a]     -- ^ The list of points to find the centroid of
                   -> PointR3
  findCentroid objs = 
    (mconcat $ map centroid objs) /. (fromIntegral $ length objs :: Double)


  -- | Computes the axis-aligned bounding box (AABB) for the given object
  aabb ::    a    -- ^ The object to compute the AABB of
          -> AABB
  aabb obj = newAABB (point3 x y z) (point3 x' y' z')
    where
      (minP,maxP) = extent obj
      (x,y,z)     = (delta $ getX minP,delta $ getY minP,delta $ getZ minP)
      (x',y',z')  = (delta $ getX maxP,delta $ getY maxP,delta $ getZ maxP)
      -- Shifts the component by epsilon the in correct direction
      delta p = p + ((signum p) * epsilon)


  -- | Like computeAABB, but over a collection of a's
  findAABB ::    [a]        -- ^ A list of objects to compute the AABB of
              -> Maybe AABB
  findAABB []   = Nothing
  findAABB objs = Just $ newAABB finalMin finalMax
    where 
      (finalMin,finalMax) = foldr compareAABB (worldOrigin,worldOrigin) objs
      compareAABB obj (currMin,currMax) = (newMin,newMax)
        where
          aabb'           = aabb obj
          (objMin,objMax) = (aabb'^.vMin,aabb'^.vMax)
          (x1,x1',x2,x2') = (getX objMin,getX currMin,getX objMax,getX currMax)
          (y1,y1',y2,y2') = (getY objMin,getY currMin,getY objMax,getY currMax)
          (z1,z1',z2,z2') = (getZ objMin,getZ currMin,getZ objMax,getZ currMax)
          newMin          = point3 (min x1 x1') (min y1 y1') (min z1 z1')
          newMax          = point3 (max x2 x2') (max y2 y2') (max z2 z2')
