{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.Space.KDTree (
   KDTree(..)
  ,KPoint(..)
  ,KVector
  ,KExtent(..)
  ,KHits(..)
  ,kdtree
  ,nodeExtent
  )
where
import Prelude hiding (foldr)
import Data.Typeable (Typeable)
import Control.Lens (makeLenses)
import Control.Lens.Getter (Getter, to)
import Data.List (partition, minimumBy)
import Text.Printf (printf)

--------------------------------------------------------------------------------

-- | KD-Tree data type
data KDTree a b = Node
  { _leftTree    :: !(KDTree a b)
  , _rightTree   :: !(KDTree a b)
  , _aabb        :: !(a,a) 
  , _treeDepth   :: !Int 
  , _splitAxis   :: !Int 
  , _splitCost   :: !Double 
  , _numChildren :: !Int }
  | Leaf [b]
  deriving (Eq,Typeable,Foldable,Functor,Traversable)

makeLenses ''KDTree


-- | Returns the AABB defining the extent for the given tree node
nodeExtent :: Getter (KDTree a b) (a,a)
nodeExtent = to _extent
  where
    _extent (Node _ _ aabb' _ _ _ _) = aabb'
    _extent _                        = error "boundingBox: Leaf"

--------------------------------------------------------------------------------

-- | Defines a point in K-dimensional space
class KPoint a where

  -- | Returns the size of K, e.g. the number of dimensions
  dimensions ::    a   -- ^ The point in K-dimensional space
                -> Int


  --  | Extracts the i-th component in K-space for the given point
  extract ::    Int    -- ^ The index of the component, starting at 0 
             -> a      -- ^ The point to extract the component from
             -> Double


  -- | Constructs a new KPoint instance from the given list of coordinates
  fromList ::    [Double] -- ^ The list of coordinates
              -> a

--------------------------------------------------------------------------------

class KVector a where {}

--------------------------------------------------------------------------------

-- | Defines the (min,max) extent of a given object in K-dimensional space
class KPoint b => KExtent a b where

  extentOf :: a -> (b,b)

--------------------------------------------------------------------------------

-- | Defines the intersection test for an AABB in K-dimensional space
class (KPoint a, KVector b) => KHits a b where

  -- | Tests if the AABB given by (a,a) is hit by a ray parameterized by the 
  -- given origin and direction
  hits :: a -> b -> (a,a) -> Bool

--------------------------------------------------------------------------------

instance Show a => Show (KDTree a b) where

  show (Node left right (boxMin,boxMax) depth axis cost children) = 
    printf "N { axis=%d, cost=%.2f, children=%d, depth=%d, aabb=<%s,%s> } ::\n %s %s\n %s %s"
      axis 
      cost 
      children 
      depth 
      (show boxMin) (show boxMax)
      sp (show left) 
      sp (show right)
    where
      sp = replicate ((depth + 1) * 2) '-' 
  show (Leaf objs) = printf "L { size=%d }" $ length objs

-------------------------------------------------------------------------------

-- | Given a list of KPoints, this function will fold the list into a tuple
-- pair based on the given summarization function
foldSummary :: 
  (KPoint a)
    => ([Double] -> (Double,Double)) -- ^ The summarization function
    -> [a]                           -- ^ The list of KPoint instances
    -> (a,a)
foldSummary _ []               = error "foldSummary: empty list"
foldSummary summarize !ps@(!p:_) = 
  let (l',r') = walk 0 (dimensions p) ([],[])
  in  (fromList l', fromList r')
  where 
    walk i k accums@(accumL,accumR)
      | i >= k    = accums
      | otherwise = 
          let ith   = fmap (extract i) ps
              (l,r) = summarize ith
          in  walk (i + 1) k $ (accumL ++ [l],accumR ++ [r])


-- | Folds 2 KPoint instances into a single KPoint using the supplied function
foldInto ::
  (KPoint a)
    => (Double -> Double -> Double) -- ^ The folding function
    -> (a,a)                        -- ^ A pair of points to operate on
    -> a
foldInto f (!x,!y) = fromList $ reverse $ walk 0 (dimensions x) []
  where
    walk i k accum
      | i >= k    = accum
      | otherwise = walk (i + 1) k $ (f x' y'):accum
      where 
        (x',y') = (extract i x,extract i y)


-- | Returns the points constituting the (minimum,maximum) findExtent of the 
-- list of KPoint instances. The (minimum,maximum) points define the extent
-- of the constructed AABB
findExtent :: 
  (KPoint a)
    => [a]   -- ^ A list of KPoint instances to calculate the centroid of
    -> (a,a)
findExtent []      = error "findExtent: empty list"
findExtent !points = foldSummary (\ps -> (minimum ps,maximum ps)) points
{-# INLINE findExtent #-}


-- | Calculates the centroid of the the AABB based on the given extents
centroid :: 
  (KPoint a)
    => (a,a) -- ^ The (minima,maxima) of the AABB to calculate the centroid of 
    -> a
centroid !points = foldInto (\i j -> (i + j) / 2) points
{-# INLINE centroid #-}


-- | Calculates the area of the AABB formed by the two KPoints given
area :: 
  (KPoint a) 
    => (a,a) -- ^ The (minimum,maximum) corners of the AABB
    -> Double
area (!x,!y) = (k' - 1.0) * (walk 0 1)
  where
    k  = dimensions x
    k' = fromIntegral k :: Double 
    walk i accum
      | i >= k    = accum
      | otherwise = 
          walk (i + 1) $ accum * ((abs $ extract i x) + (abs $ extract i y))
{-# INLINE area #-}


-- | Returns the split cost for the given node's AABB, based on the surface
-- area heuristic (SAH) cost, where
--   cost = AABB surface area * AABB primitive count 
computeSplitCost :: 
  (KPoint a)
    => (a,a)  -- ^
    -> Int    -- ^
    -> Double
computeSplitCost !points !children = 
  (fromIntegral children :: Double) * (area points) 
{-# INLINE computeSplitCost #-}

--------------------------------------------------------------------------------

-- | Determines the cheapest axis [0,k] to split the current node by
cheapestSplitAxis :: 
  (KPoint a)
    => Int     -- ^ The number of dimensions per point, k
    -> a       -- ^ The centroid that acts as the split point
    -> [(a,b)] -- ^ The (KPoint,object) instances to test
    -> (Double, Int, [(a,b)], [(a,b)])
cheapestSplitAxis !k !center !pointsObjs = 
  minimumBy compareCost $ fmap perAxis [0 .. (k - 1)]
  where
    -- | Compares one (cost,axis,leftChildren,rightChildren) tuple to another
    -- by cost
    compareCost (leftCost,_,_,_) (rightCost,_,_,_) = compare leftCost rightCost

    -- | Given two lists of children, left and right, this function computes
    -- the total cost based on the size of the calculated AABB and the number
    -- of child objects present it
    computeCost [] []      = error "cheapestSplitAxis: empty lists <left,right>"
    computeCost left []    = computeSplitCost (findExtent left) $ length left
    computeCost [] right   = computeSplitCost (findExtent right) $ length right
    computeCost left right = leftCost + rightCost
      where
        leftCost  = computeSplitCost (findExtent left) $ length left
        rightCost = computeSplitCost (findExtent right) $ length right

    -- | Calculates (cost,axis,leftChildren,rightChildren) for the given axis
    perAxis axis = (totalCost,axis,leftOfAxis,rightOfAxis)
      where 
        axisCoordinate           = extract axis center
        (leftOfAxis,rightOfAxis) = 
          partition (\(p,_) -> extract axis p <= axisCoordinate) pointsObjs
        (leftPoints,rightPoints) = (fmap fst leftOfAxis,fmap fst rightOfAxis)
        totalCost                = computeCost leftPoints rightPoints


-- | Internal: builds a new KD-Tree instance 
build :: 
  (KPoint a, KExtent b a) 
    => Int       -- ^ Number of dimensions, k
    -> (Int,Int) -- ^ (depth,maxDepth)
    -> Int       -- ^ The maximum number of children allowed per node before a
                 --   split occurs
    -> [(a,b)]   -- ^ The (KPoint,object) instances to build the tree out of
    -> KDTree a b
build !k (!depth,!maxDepth) !maxPerChildrenNode !pointsObjs
   | (null pointsObjs) || 
      (depth >= maxDepth) || 
        (countChildren < maxPerChildrenNode) = Leaf $ fmap snd pointsObjs
   | otherwise = Node (build k depth' maxPerChildrenNode leftChildren) 
                      (build k depth' maxPerChildrenNode rightChildren) 
                      computedAABB
                      depth
                      onAxis
                      withCost
                      countChildren
  where
    pair2List (x,y) = [x,y]
    countChildren = length pointsObjs
    depth'        = (depth + 1, maxDepth)
    computedAABB  = findExtent $ concatMap (pair2List . extentOf . snd) pointsObjs
    center        = centroid computedAABB
    (withCost, onAxis,leftChildren,rightChildren) = 
      cheapestSplitAxis k center pointsObjs


-- | Builds a new KD-Tree instance 
kdtree :: 
  (KPoint a, KExtent b a) 
    => [(a,b)]    -- ^ The (KPoint,object) instances to build the tree out of
    -> Int        -- ^ Maximum depth of the tree
    -> Int        -- ^ Maximum number of points per leaf node
    -> KDTree a b
kdtree [] _ _                                = Leaf []
kdtree !pointsObjs@(p:_) maxDepth maxPerNode = 
  build (dimensions $ fst p) (0,maxDepth) maxPerNode pointsObjs

--------------------------------------------------------------------------------

