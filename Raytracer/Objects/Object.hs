{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.Objects.Object (WorldObject(..)
  ,createObject
  ,seqId, typeName, attributes)
where
import Text.Printf (printf)
import qualified Data.DList as DList
import Control.Lens hiding (children, index, traverse)
import Raytracer.Space.Tensor (R3(..), PointR3, VectorR3, point3
  ,AffineTransformable(..))
import qualified Raytracer.Objects.Primitive as P
import qualified Raytracer.Space.Tensor as T
import qualified Raytracer.Space.KDTree as KDTree
import qualified Raytracer.World.AABB as AABB
import qualified Raytracer.World.Intersect as I
import qualified Raytracer.Objects.Attributes as A

--------------------------------------------------------------------------------

-- | Objects with more than objThreshold primitives will use a KD-tree
-- for indexing
objThreshold :: Int
objThreshold = 5

--------------------------------------------------------------------------------

-- | This type represents a single, cohesive object unit in the world. A
-- | WorldObject can consist or a single, atomic primitive object like a sphere
-- | for instance, a number of primitive objects combined together in the case
-- | of a CSG object, or potentially thousands of primitive objects, like a 
-- | a triangle mesh.
data WorldObject a =
    BasicObject Int String [a] AABB.AABB A.Attributes
  | ComplexObject Int String [a] (KDTree.KDTree PointR3 a) A.Attributes

--------------------------------------------------------------------------------

-- | Builds a KD-Tree spatial index on the given list of objects, using
-- a default max depth of 20 and max children-per-node limit of 5 objects
buildIndex ::
  (KDTree.KExtent a PointR3
  ,AABB.WithAABB a) 
    => [a] 
    -> KDTree.KDTree PointR3 a
-- Key is the centroid of the object, value is the object itself:
-- => [(centroid(obj),obj)]
buildIndex objs = 
  KDTree.kdtree (map (\obj -> (AABB.centroid obj,obj)) objs) 20 5


-- | Simplified constructor for WorldObject that automatically calculates
-- the maximum AABB for the given collection of objects
createObject :: 
  (I.Intersectable a a
  ,AABB.WithAABB a
  ,KDTree.KExtent a PointR3) 
  => Int           -- ^ Object sequence ID
  -> String        -- ^ Object type name
  -> [a]           -- ^ The child objects contained in this parent object
  -> A.Attributes  -- ^ The attribute that apply to this object
  -> WorldObject a
createObject !seqId' !typeName' !children !attrs
  | length children < objThreshold = createBasic
  | otherwise                      = createComplex
  where
    createBasic   = 
      case (AABB.findAABB children) of
        Just bbox -> BasicObject seqId' typeName' children bbox attrs 
        Nothing   -> error "createObject: empty list of objects"
    createComplex = ComplexObject seqId' typeName' children (buildIndex children) attrs


seqId :: Lens' (WorldObject a) Int
seqId = lens getter setter
  where
    getter (BasicObject seqId' _ _ _ _)   = seqId'
    getter (ComplexObject seqId' _ _ _ _) = seqId'
    setter _ = error "Can't modify object sequence ID"


typeName :: Lens' (WorldObject a) String
typeName = lens getter setter
  where
    getter (BasicObject _ typeName' _ _ _)                       = typeName'
    getter (ComplexObject _ typeName' _ _ _)                     = typeName'
    setter (BasicObject !seqId' _ !cs !bbox !attrs) !typeName'    = BasicObject seqId' typeName' cs bbox attrs
    setter (ComplexObject !seqId' _ !objs !idx !attrs) !typeName' = ComplexObject seqId' typeName' objs idx attrs


attributes :: Lens' (WorldObject a) A.Attributes
attributes = lens getter setter
  where
    getter (BasicObject _ _ _ _ !attrs)                          = attrs
    getter (ComplexObject _ _ _ _ !attrs)                        = attrs
    setter (BasicObject !seqId' !typeName' !cs !bbox _) !attrs'   = BasicObject seqId' typeName' cs bbox attrs'
    setter (ComplexObject !seqId' !typeName' objs !idx _) !attrs' = ComplexObject seqId' typeName' objs idx attrs'

--------------------------------------------------------------------------------

instance Eq (WorldObject a) where

  (==) a b = (a^.seqId) == (b^.seqId) && (a^.typeName) == (b^.typeName)


instance (Show a) => Show (WorldObject a) where

  show obj@(BasicObject _ _ cs _ _) = printf "%s<%d>%s" (obj^.typeName) (obj^.seqId) (show cs)
  show obj = printf "%s<%d>[...]" (obj^.typeName) (obj^.seqId)

--------------------------------------------------------------------------------

instance I.Intersectable (WorldObject P.Primitive) P.Primitive where

  intersects !ray' !obj@(BasicObject _ _ !children _ _) attrs = 
    case (I.closestLocalIntersection ray' children attrs) of
      Just rayHit -> 
        Just $ I.createIntersection 
                 ray' 
                 (rayHit^.I.hitAt,rayHit^.I.hitAtLocal)
                 (rayHit^.I.normal)
                 obj
                 (rayHit^.I.intersectedChild)
      Nothing -> Nothing

  intersects ray' obj@(ComplexObject _ _ _ idx _) attrs
    | null maybeHits = Nothing
    | otherwise      = 
        case (I.closestLocalIntersection ray' maybeHits attrs) of
          Just rayHit -> 
            Just $ I.createIntersection 
                     ray' 
                     (rayHit^.I.hitAt,rayHit^.I.hitAtLocal)
                     (rayHit^.I.normal)
                     obj
                     (rayHit^.I.intersectedChild)
          Nothing -> Nothing
    where
      -- |
      --m  = attrs^.A.affine^.T.mBase
      mi = attrs^.A.affine^.T.mInv
      o  = (ray'^.I.origin)    <!- mi
      d  = (ray'^.I.direction) <!- mi
      traverse (KDTree.Node left right bbox _ _ _ _)
        | KDTree.hits o d bbox = DList.append (traverse left) (traverse right)
        | otherwise            = DList.empty
      traverse (KDTree.Leaf objs) = DList.fromList objs

      -- |
      maybeHits = DList.toList $ traverse $ idx

--------------------------------------------------------------------------------

instance AffineTransformable (WorldObject P.Primitive) where

  -- Primarily, this transforms the bounding box (AABB) defined for the 
  -- WorldObject. The primitives/whatever contained in the WorldObject
  -- are transformed (if at all) in their respective intersection code 
  -- implementations
  (<!-) (BasicObject !seqId' !typeName' !objs _ !attrs) !t = 
      case AABB.findAABB objs of
        Just bb' -> BasicObject seqId' typeName' objs (bb' <!- m') newAttrs
        Nothing  -> error "(<!-): empty object list"
    where
      m        = attrs^.A.affine.T.mBase
      newAttrs = A.affine .~ (T.applyTransforms t m) $ attrs
      m'       = newAttrs^.A.affine.T.mBase

  (<!-) (ComplexObject !seqId' !typeName' !children !index !attrs) !t = 
    ComplexObject seqId' typeName' children index newAttrs
    where
      m        = attrs^.A.affine.T.mBase
      newAttrs = A.affine .~ (T.applyTransforms t m) $ attrs
      --m'       = newAttrs^.A.affine.T.mBase

--------------------------------------------------------------------------------

instance A.WithAttributes (WorldObject a) where

  getAttributes obj = obj^.attributes

--------------------------------------------------------------------------------

instance AABB.WithAABB (WorldObject a) where

  extent (BasicObject _ _ _ !bbox _)  = (bbox^.AABB.vMin,bbox^.AABB.vMax)
  extent (ComplexObject _ _ _ !idx _) = idx^.KDTree.nodeExtent
  {-# INLINE extent #-}

  aabb (BasicObject _ _ _ !bbox _)   = bbox
  aabb (ComplexObject _ _ _ !idx _)  = AABB.newAABB minP maxP
    where
      (minP,maxP) = idx^.KDTree.nodeExtent
  {-# INLINE aabb #-}

--------------------------------------------------------------------------------

instance KDTree.KPoint PointR3 where

  dimensions _ = 3
  {-# INLINE dimensions #-}

  extract 0 !p = getX p
  extract 1 !p = getY p
  extract 2 !p = getZ p
  extract !i _ = error $ "get index out of range (" ++ (show i) ++ ")"
  {-# INLINE extract #-}

  fromList [!x,!y,!z] = point3 x y z
  fromList bad        = error $ "expected [x,y,z]; got " ++ (show bad)
  {-# INLINE fromList #-}


instance KDTree.KVector VectorR3


instance KDTree.KExtent P.Primitive PointR3 where

  extentOf = AABB.extent
  {-# INLINE extentOf #-}


instance KDTree.KHits PointR3 VectorR3 where

  hits = AABB.hits
  {-# INLINE hits #-}
