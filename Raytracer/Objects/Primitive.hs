{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Raytracer.Objects.Primitive (Primitive(..))
where
--import Debug.Trace (trace)
--import Text.Printf (printf)
import Data.Tuple (swap)
import Control.Lens ((^.))
import Raytracer.Space.Tensor (R3(..), PointR3, point3, VectorR3, vec3
  ,dotpv, (<!-), (|->), (/=>), (/->), (*.)
  ,cross, barycenter, mBase, mInv, mInvT, AffineTransformable(..))
import Raytracer.World.AABB (WithAABB(..))
import Raytracer.World.Intersect (Intersectable(..)
  ,createIntersection, origin, direction)
import Raytracer.World.Environment (worldOrigin, maxDist)
import Raytracer.Objects.Attributes (affine)
import Raytracer.Utils.Math (discriminant, epsilon)

--------------------------------------------------------------------------------

-- | The Primitive specifies object primitives in the world space. Primitives
-- | are the indivisible units of object construction in world space
data Primitive =

  -- Simple parameteric sphere
    Sphere !Double -- ^ The center & radius of the sphere

  -- Simple parameteric  plane
  | Plane !(VectorR3,Double)   -- ^ Plane normal, i.e. vector orthogonal to the 
                               -- surface of the plane and the distance from the 
                               -- origin
          !(VectorR3,VectorR3) -- U-axis/V-axis vectors

  -- Simple parameteric box
  | Box !(PointR3,PointR3) -- ^ The minimum/maximum extents of the box

  -- Simple parameteric triangle
  | Triangle !(PointR3,PointR3,PointR3)    -- ^ The vertices of the triangle
             !(VectorR3,VectorR3)          -- ^ Edges 1 & 2 sharing vertex #1
             !(VectorR3,VectorR3,VectorR3) -- ^ Vertex normals

  deriving (Show)

--------------------------------------------------------------------------------

instance Intersectable Primitive Primitive where

  -- | > Sphere case
  -- | Adapted from the C++ implementation at 
  -- * http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection
  intersects ray obj@(Sphere r) attrs
    | d < 0 || v1 < 0 = Nothing
    | otherwise       = 
        let lp   = o |-> (dir *. v') -- local
            wp   = lp          <!- m -- world space
            ctr  = worldOrigin <!- m
            n    = ctr /-> wp
        in  Just $ createIntersection ray (wp,lp) n obj obj
    where
      m          = attrs^.affine^.mBase
      mi         = attrs^.affine^.mInv
      o          = (ray^.origin)    <!- mi
      dir        = (ray^.direction) <!- mi
      a          = dir |. dir
      b          = 2.0 * (o `dotpv` dir)
      c          = (o |. o) - r^2
      d          = discriminant a b c
      q          = (if b < 0 then (-b + sqrt d) else (-b - sqrt d)) / 2.0
      (v0',v1')  = (q / a, c / q)
      (v0,v1)    = (min v0' v1', max v0' v1')
      v'         = if v0 < 0 then v1 else v0

  -- | > Plane case
  -- | Adapted from code at 
  -- * http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_1_Introduction.shtml
  intersects ray obj@(Plane (n,d) _) _
    | d' /= 0 && dist > 0 = Just $ createIntersection ray (p,p) n obj obj
    | otherwise           = Nothing
    where
      --mit  = attrs^.affine^.mInvT
      p    = o |-> (dir *. dist) 
      o    = ray^.origin
      dir  = ray^.direction
      d'   = n |. dir
      dist = -((o `dotpv` n) + d) / d'

  -- | > Triangle case
  -- | Möller–Trumbore algorithm
  -- * http://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
  intersects ray obj@(Triangle vertices@(v0,_,_) (e1,e2) (vn0,vn1,vn2)) attrs
    | det > -epsilon && det < epsilon = Nothing
    | u < 0.0 || u > 1.0              = Nothing
    | v < 0.0 || (u + v) > 1.0        = Nothing
    | otherwise = 
        if t' > epsilon
          then let lp            = o |-> (dir *. t')
                   wp            = lp <!- m
                   (wv0,wv1,wv2) = barycenter lp vertices -- weights
                   n             = (vn0 *. wv0) + (vn1 *. wv1) + (vn2 *. wv2)
               in  Just $ createIntersection ray (wp,lp) n obj obj
          else Nothing
    where
      (m,mi,mit) = attrs^.affine
      o    = (ray^.origin)    <!- mi
      dir  = (ray^.direction) <!- mi
      p    = dir `cross` e2
      det  = e1 |. p
      det' = 1.0 / det
      t    = o /=> v0
      u    = (t |. p) * det'
      q    = t `cross` e1
      v    = (dir |. q) * det'
      t'   = (e2 |. q) * det'

  -- > Box case
  -- | Adapted from code in
  -- * "Ray Tracing from the Ground Up" by Kevin Suffern, 2007
  intersects ray obj@(Box (v0,v1)) attrs
    | near > far || far < 0 = Nothing
    | otherwise = 
        let (e,(tx,ty,tz)) = if near < 0 
                             then (far,(x',y',z')) 
                             else (near,(x,y,z))
            n  = (findNormal e $ point3 tx ty tz) <!- mit
            lp = o |-> (d *. e) -- point in local space
            wp = lp <!- m       -- transformed point in world space
        in  Just $ createIntersection ray (wp,lp) n obj obj
      where
        findNormal t p
          | abs ((getX p) - t) <= epsilon = vec3 (signum $ getX p) 0 0
          | abs ((getY p) - t) <= epsilon = vec3 0 (signum $ getY p) 0
          | otherwise                     = vec3 0 0 (signum $ getZ p)
        checkAndSwap v
          | v > 0     = id 
          | otherwise = swap
        (m,mi,mit) = attrs^.affine
        o          = (ray^.origin)    <!- mi
        d          = (ray^.direction) <!- mi
        (ox,oy,oz) = (getX o,getY o,getZ o)
        (dx,dy,dz) = (getX d,getY d,getZ d)
        (x,x')     = 
          checkAndSwap dx ((getX v0 - ox) / getX d, (getX v1 - ox) / dx)
        (y,y')     =  
          checkAndSwap dy ((getY v0 - oy) / dy, (getY v1 - oy) / dy)
        (z,z')     = 
          checkAndSwap dz ((getZ v0 - oz) / dz, (getZ v1 - oz) / dz)
        near = max x $ max y z
        far  = min x' $ min y' z'

--------------------------------------------------------------------------------

instance WithAABB Primitive where

  centroid (Sphere _) = worldOrigin

  centroid (Triangle (v0,v1,v2) _ _) = 
    point3 ((v0x + v1x + v2x) / 3)
           ((v0y + v1y + v2y) / 3)
           ((v0z + v1z + v2z) / 3)
    where
      (v0x,v0y,v0z) = (getX v0,getY v0,getZ v0)
      (v1x,v1y,v1z) = (getX v1,getY v1,getZ v1)
      (v2x,v2y,v2z) = (getX v2,getY v2,getZ v2)

  -- Everything else uses the defualt implementation
  centroid obj = centroid obj


  extent (Sphere r) = (point3 (cx - r) (cy - r) (cz - r)
                      ,point3 (cx + r) (cy + r) (cz + r))
    where 
      (cx,cy,cz) = (getX worldOrigin,getY worldOrigin,getZ worldOrigin)

  extent (Plane (_,d) _) = (point3 (-maxDist) (d - epsilon) (-maxDist)
                           ,point3 maxDist (d + epsilon) maxDist)

  extent (Box (v0,v1)) = (v0,v1)

  extent (Triangle (v0,v1,v2) _ _) = 
    (point3 minX minY minZ,point3 maxX maxY maxZ)
    where
      (v0x,v0y,v0z) = (getX v0,getY v0,getZ v0)
      (v1x,v1y,v1z) = (getX v1,getY v1,getZ v1)
      (v2x,v2y,v2z) = (getX v2,getY v2,getZ v2)
      minX = min v0x $ min v1x v2x
      minY = min v0y $ min v1y v2y
      minZ = min v0z $ min v1z v2z
      maxX = max v0x $ max v1x v2x
      maxY = max v0y $ max v1y v2y
      maxZ = max v0z $ max v1z v2z
