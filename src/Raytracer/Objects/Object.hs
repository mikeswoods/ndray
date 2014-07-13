{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Raytracer.Objects.Object 
  (
   WorldObject
  ,PrimBox
  ,BoundingVolume
  ,createObject
  ,seqId
  ,typeName
  ,attributes
  )
where
import Data.Typeable (Typeable)
import Text.Printf (printf)
import qualified Data.DList as DList
import Control.Lens hiding (children, index, traverse)
import Raytracer.Space.Tensor (R3(..), PointR3, VectorR3, point3
  ,AffineTransformable(..))
--import Raytracer.Const
import qualified Raytracer.Space.Tensor as Tensor
import qualified Raytracer.Space.KDTree as KDTree
import qualified Raytracer.World.AABB as AABB
import qualified Raytracer.World.Intersect as Intersect
import qualified Raytracer.Objects.Attributes as Attributes

--------------------------------------------------------------------------------

-- (Sane) KD-Tree related default values

-- | World objects with more than N primitives will use a KD-tree
-- for indexing
maxPrimitivesThreshold :: Int
maxPrimitivesThreshold = 5


-- | Controls the maximum number of children/primitives per KD-tree leaf
maxKDTreeChildrenPerLeaf :: Int
maxKDTreeChildrenPerLeaf = 20


-- | Controls the maximum depth of the constructed KD-tree
maxKDTreeDepth :: Int
maxKDTreeDepth = 5

--------------------------------------------------------------------------------

-- Primitive box that holds 1 primitive object
data PrimBox = 
  forall a. (Intersect.Intersectable a,AABB.WithAABB a,KDTree.KExtent a PointR3)
    => PrimBox a
  deriving (Typeable)


-- | Bounding volume type
data BoundingVolume = 
    Box AABB.AABB
  | Tree (KDTree.KDTree PointR3 PrimBox)
  deriving (Typeable)

-- | This type represents a single, cohesive object unit in the world. A
-- | WorldObject can consist or a single, atomic primitive object like a sphere
-- | for instance, a number of primitive objects combined together in the case
-- | of a CSG object, or potentially thousands of primitive objects, like a 
-- | a triangle mesh.

data WorldObject = 
  WorldObject Int String [PrimBox] BoundingVolume Attributes.Attributes
  deriving (Typeable)

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
buildIndex !objs = 
  KDTree.kdtree (fmap (\obj -> (AABB.centroidOf obj,obj)) objs) 
                maxKDTreeChildrenPerLeaf 
                maxKDTreeDepth


-- | Simplified constructor for WorldObject that automatically calculates
-- the maximum AABB for the given collection of objects
createObject :: 
  (Intersect.Intersectable a
  ,AABB.WithAABB a
  ,KDTree.KExtent a PointR3) 
  => Int                   -- ^ Object sequence ID
  -> String                -- ^ Object type name
  -> [a]                   -- ^ The child objects contained in this parent object
  -> Attributes.Attributes -- ^ The attribute that apply to this object
  -> WorldObject
createObject !seqId' !typeName' !children !attrs =
  WorldObject seqId' typeName' primitives pickBoundingVolume attrs
  where
    primitives         = fmap PrimBox children
    pickBoundingVolume = 
      if   length children < maxPrimitivesThreshold
      then case (AABB.findAABB children) of
             Just aabb -> Box aabb
             Nothing   -> error "createObject: empty list of objects"
      else Tree $ buildIndex primitives


seqId :: Lens' WorldObject Int
seqId = lens getter setter
  where
    getter !(WorldObject !seqId' _ _ _ _) = seqId'
    setter  _                             = error "Can't modify object sequence ID"


typeName :: Lens' WorldObject String
typeName = lens getter setter
  where
    getter !(WorldObject _ !typeName' _ _ _)                  = typeName'
    setter !(WorldObject !seqId' _ !cs !bv !attrs) !typeName' = WorldObject seqId' typeName' cs bv attrs


attributes :: Lens' WorldObject Attributes.Attributes
attributes = lens getter setter
  where
    getter !(WorldObject _ _ _ _ !attrs)                       = attrs
    setter !(WorldObject !seqId' !typeName' !cs !bv _) !attrs' = WorldObject seqId' typeName' cs bv attrs'

--------------------------------------------------------------------------------

instance Eq WorldObject where

  (==) a b = (a^.seqId) == (b^.seqId) && (a^.typeName) == (b^.typeName)


instance Show WorldObject where

  show obj = printf "%s<%d>" (obj^.typeName) (obj^.seqId)

--------------------------------------------------------------------------------

instance Intersect.Intersectable PrimBox where

  intersects ray' (PrimBox p) = Intersect.intersects ray' p


instance Intersect.Intersectable WorldObject where

  intersects !ray' !(WorldObject _ _ !children !(Box _) _) !attrs = 
    case (Intersect.closestLocalIntersection ray' children attrs) of
      Just (_,rayHit) -> Just rayHit
      Nothing         -> Nothing

  intersects !ray' !(WorldObject _ _ _ !(Tree kdt) _) !attrs
    | null maybeHits = Nothing
    | otherwise      = 
        case (Intersect.closestLocalIntersection ray' maybeHits attrs) of
          Just (_,rayHit) -> Just rayHit
          Nothing         -> Nothing
    where
      mi = attrs^.Attributes.affine^.Tensor.mInv
      o  = (ray'^.Intersect.origin)    <!- mi
      d  = (ray'^.Intersect.direction) <!- mi
      traverse' (KDTree.Node left right bbox _ _ _ _)
        | KDTree.hits o d bbox    = DList.append (traverse' left) (traverse' right)
        | otherwise               = DList.empty
      traverse' (KDTree.Leaf objs) = DList.fromList objs
      maybeHits                   = DList.toList $ traverse' $ kdt

--------------------------------------------------------------------------------

instance AffineTransformable WorldObject where

  -- Primarily, this transforms the bounding box (AABB) defined for the 
  -- WorldObject. The primitives/whatever contained in the WorldObject
  -- are transformed (if at all) in their respective intersection code 
  -- implementations
  (<!-) (WorldObject !seqId' !typeName' !cs !(Box _) !attrs) !t = 
      case AABB.findAABB cs of
        Just aabb' -> WorldObject seqId' typeName' cs (Box (aabb' <!- m')) newAttrs
        Nothing    -> error "(<!-): empty object list"
    where
      m        = attrs^.Attributes.affine.Tensor.mBase
      newAttrs = Attributes.affine .~ (Tensor.applyTransforms t m) $ attrs
      m'       = newAttrs^.Attributes.affine.Tensor.mBase

  (<!-) (WorldObject !seqId' !typeName' !children !(Tree kdt) !attrs) !t = 
    WorldObject seqId' typeName' children (Tree kdt) newAttrs
    where
      m        = attrs^.Attributes.affine.Tensor.mBase
      newAttrs = Attributes.affine .~ (Tensor.applyTransforms t m) $ attrs
      --m'       = newAttrs^.Attributes.affine.Tensor.mBase

--------------------------------------------------------------------------------

instance Attributes.WithAttributes WorldObject where

  attributesOf obj = obj^.attributes
  {-# INLINE attributesOf #-}

--------------------------------------------------------------------------------

instance AABB.WithAABB PrimBox where

  extentOf (PrimBox p) = AABB.extentOf p
  {-# INLINE extentOf #-}

  boundingBoxOf (PrimBox p) = AABB.boundingBoxOf p
  {-# INLINE boundingBoxOf #-}

instance AABB.WithAABB WorldObject where

  extentOf (WorldObject _ _ _ !(Box aabb) _) = (aabb^.AABB.vMin,aabb^.AABB.vMax)
  extentOf (WorldObject _ _ _ !(Tree kdt) _) = kdt^.KDTree.nodeExtent
  {-# INLINE extentOf #-}

  boundingBoxOf (WorldObject _ _ _ !(Box aabb) _) = aabb
  boundingBoxOf (WorldObject _ _ _ !(Tree kdt) _) = AABB.newAABB vMin vMax
    where
      (vMin,vMax) = kdt^.KDTree.nodeExtent
  {-# INLINE boundingBoxOf #-}

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


instance (AABB.WithAABB a) => KDTree.KExtent a PointR3 where

  extentOf = AABB.extentOf
  {-# INLINE extentOf #-}


instance KDTree.KHits PointR3 VectorR3 where

  hits = AABB.hits
  {-# INLINE hits #-}
