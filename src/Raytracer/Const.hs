{-# OPTIONS -Wall #-}

module Raytracer.Const
where

--------------------------------------------------------------------------------

-- (Sane) KD-Tree related default values

-- | World objects with more than N primitives will use a KD-tree
-- for indexing
_maxPrimitivesThreshold :: Int
_maxPrimitivesThreshold = 5


-- | Controls the maximum number of children/primitives per KD-tree leaf
_maxKDTreeChildrenPerLeaf :: Int
_maxKDTreeChildrenPerLeaf = 20


-- | Controls the maximum depth of the constructed KD-tree
_maxKDTreeDepth :: Int
_maxKDTreeDepth = 5

--------------------------------------------------------------------------------