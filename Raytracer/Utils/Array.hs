{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Raytracer.Utils.Array (index1, index2, interpolate)
where
import Raytracer.Utils.Display (getInterpolationScreenSize) 
import Data.Array.Repa 
import Data.Array.Repa.Operators.Traversal (unsafeTraverse)

-- | For an array of dimensions (m,n), this function maps a 2-dimensional
-- index (i,j) to a linear index k
index1 ::    (Int,Int) -- ^ The (m,n) dimensions of the array
          -> (Int,Int) -- ^ The (i,j)-th index into the array
          -> Int
index1 (w,_) (i,j) = (w * j) + i


-- | For an array of dimensions (m,n), this function maps a 1-dimensional
-- index k to a 2-dimensional index (i,j)
index2 ::    (Int,Int) -- ^
          -> Int       -- ^
          -> (Int,Int)
index2 (w,_) i = (x,y)
  where
    y = i `div` w
    x = i - (y * w)


-- | This function takes an array and interpolates its values by simple
-- substitution according to the given block size
interpolate :: (Source r b)
            => Int            -- ^ The blocksize
            -> Array r DIM2 b -- ^ The input array
            -> Array D DIM2 b
interpolate !blockSize !src = unsafeTraverse src resize update
  where
    resize (Z :. x :. y) = (Z :. x' :. y')
      where 
        (x',y') = getInterpolationScreenSize (x,y) blockSize
    update lookup' (Z :. i :. j) = 
      lookup' (ix2 (i `div` blockSize) (j `div` blockSize))


-- | Adapted from the great REPA tutorial at
-- http://www.cse.chalmers.se/edu/course/DAT280_Parallel_Functional_Programming/Papers/tutorialWinner.pdf
--
-- Generate a write-ready color array from any coloring function
-- and array of Doubles
--colorArray ::    Array D DIM2 C.WorldColor
--              -> Array D DIM3 Word8
--colorArray xs = R.traverse xs resize update
--  where resize (Z :. x :. y)             = (Z :. x :. y :. 4)
--        update lookup (Z :. x :. y :. c) =
--          let col = lookup (ix2 x y) 
--          in case c of
--               0 -> col^.C.r -- R
--               1 -> col^.C.g -- G
--               2 -> col^.C.b -- B
--               3 -> 0        -- A
