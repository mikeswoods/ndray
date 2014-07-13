{-# OPTIONS -Wall #-}

module Raytracer.World.Const (Width, Height, Depth
  ,R2Coord, R3Coord
  ,maxDist)
where

--------------------------------------------------------------------------------

-- | Width is an integral value
type Width = Int


-- | Height is an integral value
type Height = Int


-- | Recursion depth is an integral value
type Depth = Int


-- | Simple type synonym for a generic (x,y) tuple with components in R
type R2Coord = (Double, Double)


-- | Simple type synonym for a generic (x,y,z) tuple with components in R
type R3Coord = (Double, Double, Double)

--------------------------------------------------------------------------------

-- | The world maximum distance
maxDist :: Double
maxDist = 1.0e5

--------------------------------------------------------------------------------