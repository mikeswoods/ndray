{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-name-shadowing #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.World.Intersect (origin, direction
  ,ray, hitAt, hitAtLocal, dist, normal
  ,flipNormal, bump, createRay, createIntersection
  ,closestLocalIntersection, closestSceneIntersection
  ,Ray, RayHit, Intersectable(..))
where
--import Debug.Trace (trace)
import Data.Maybe
import Data.Foldable hiding (minimumBy)
import Text.Printf (printf)
import Control.Monad
import Control.Lens ((^.), makeLenses)
import Raytracer.Objects.Attributes (Attributes, WithAttributes(..))
import Raytracer.Space.Tensor hiding (direction, dist)
import qualified Raytracer.Space.Tensor as Tensor
import Raytracer.Utils.Math (epsilon)

--------------------------------------------------------------------------------

-- | Basic light ray
data Ray = MkRay
  { _origin    :: PointR3  -- ^ The origin of the ray
  , _direction :: VectorR3 -- ^ The normalized direction of the ray
  } deriving (Eq)

makeLenses ''Ray

instance Show Ray where

  show ray = printf "%s |-> %s" (show $ ray^.origin) (show $ ray^.direction) 


-- | Creates a new Ray instance, using the given origin and direction. This
-- function normalizes the given direction vector prior to building a new
-- Ray instance 
createRay ::    PointR3  -- ^ The origin of the ray
             -> VectorR3 -- ^ The un-normalized directional vector
             -> Ray
createRay o d = 
  MkRay { _origin    = o
        , _direction = unit d }


-- | Ray <: AffineTransformable
instance AffineTransformable Ray where

  ray <!- m = MkRay { _origin    = (ray^.origin)    <!- m
                    , _direction = (ray^.direction) <!- m }

--------------------------------------------------------------------------------

-- | RayHit represents an instance of a ray/object intersection
data RayHit = MkRayHit
  { _ray        :: Ray
  , _hitAt      :: PointR3
  , _hitAtLocal :: PointR3
  , _normal     :: VectorR3
  , _dist       :: Double
  } deriving (Show,Eq)

makeLenses ''RayHit


-- | Creates an instance of a ray/object intersection
createIntersection ::    
     Ray               -- ^ The ray that generated the hit
  -> (PointR3,PointR3) -- ^ World intersection point, local intersection point)
  -> VectorR3          -- ^ The normal at the point of intersection
  -> RayHit
createIntersection r (wp,lp) n = 
  MkRayHit { _ray        = r
           , _hitAt      = wp
           , _hitAtLocal = lp
           , _normal     = unit $ flipNormal (r^.direction) n
           , _dist       = Tensor.dist (r^.origin) wp }

--------------------------------------------------------------------------------

-- | Any object that can be intersected with a Ray must implement this
-- type class in order to test for ray/object intersection
class Intersectable a where

  -- | Tests if an intersection occurs with the given ray and object
  intersects ::    
       Ray              -- ^ The incoming ray to test
    -> a                -- ^ The object to test for intersection
    -> Attributes       -- ^ Attributes associated with the object
    -> Maybe RayHit

--------------------------------------------------------------------------------

-- |
closestIntersection :: 
  (Intersectable a) 
    => (Ray -> a -> Maybe RayHit) -- ^ The intersection test
    -> Ray                        -- ^ The incoming ray to test
    -> [a]                        -- ^ The scene objects to test
    -> Maybe (a,RayHit)
closestIntersection findHit ray objs
  | null objs = Nothing
  | otherwise = 
      case closest of
        [h]    -> Just h
        (h:hs) -> Just $ foldl' d h hs
        []     -> Nothing
  where
    closest = do
      obj <- objs
      let hit = findHit ray obj
      guard $ isJust hit
      return (obj,fromJust hit)
    d t1@(_,hit1) t2@(_,hit2) = if (hit1^.dist) < (hit2^.dist) then t1 else t2


-- | Within a bounded container, like an AABB, this function finds the closest 
-- intersection between the ray and every child object
closestLocalIntersection ::
  (Intersectable a) 
    => Ray        -- ^ The ray to test
    -> [a]        -- ^ Objects to test
    -> Attributes -- ^ 
    -> Maybe (a,RayHit)
closestLocalIntersection ray objs attrs = closestIntersection (\r o -> intersects r o attrs) ray objs


-- | Finds the closest intersection, if it exists for the given ray in the 
-- provided list of bounding objects
closestSceneIntersection :: 
  (WithAttributes a
  ,Intersectable a) 
    => Ray          -- ^ The incoming ray to test
    -> [a]          -- ^ The scene objects to test
    -> Maybe (a,RayHit)
closestSceneIntersection = closestIntersection (\r o -> intersects r o $ attributesOf o)

--------------------------------------------------------------------------------

-- | Corrects the direction of a normal if needed if the dot product of the
-- normal and the supplied ray direction is less than or equal to zero.
flipNormal :: VectorR3 -> VectorR3 -> VectorR3
flipNormal dir norm
  | dir |. norm < 0 = -norm
  | otherwise       = norm
{-# INLINE flipNormal #-}


-- | AS the name implies, this function shifts the given position of the ray
-- by a small fixed amount, epsilon. This is needed in some cases to prevent 
-- so-called "screen-acne" or "cancer" in the rendered output that manifests 
-- as random black noise pixels. The problem is the result of floating point 
-- rounding errors
bump :: Ray -> Ray
bump r = createRay (o |-> (d *. epsilon)) d
  where
    o = r^.origin
    d = r^.direction
{-# INLINE bump #-}
