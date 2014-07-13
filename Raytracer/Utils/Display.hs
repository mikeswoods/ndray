{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Raytracer.Utils.Display (getInterpolationScreenSize) 
where

--------------------------------------------------------------------------------

-- Given screen dimensions (x,y) and an interpolation block size, this
-- function returns a new screen size (x',y'), where each dimension is
-- divisible by blockSize 
getInterpolationScreenSize ::    (Int,Int) -- ^ The original (x,y) dimensions
                              -> Int       -- ^ The blocksize
                              -> (Int,Int)
getInterpolationScreenSize (x,y) bs = (xf * bs ^ 2, yf * bs ^ 2)
  where
    bs' = (fromIntegral bs) :: Double
    x'  = (fromIntegral x) :: Double
    y'  = (fromIntegral y) :: Double
    xf  = ceiling $ x' / bs'
    yf  = ceiling $ y' / bs'
