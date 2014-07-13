{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances  #-}

module Raytracer.Utils.Math (inf, epsilon, clamp, unitRange, negUnitRange
  ,deg2rad, discriminant
  ,w2i, i2w, median, lerp, ilerp, cerp, cerp')
where
import Data.Word (Word8)

--------------------------------------------------------------------------------

-- | The special Double "infinity" value
inf :: Double
inf = read "Infinity" :: Double


-- | The predefined error roundoff threshold
epsilon :: Double
epsilon = 1.0e-4


-- | Predefined radian value
radian :: Floating a => a
radian = pi / 180.0
{-# INLINE radian #-}

--------------------------------------------------------------------------------

-- | Converts a Word8 instance to an int
w2i :: Word8 -> Int
w2i w = fromIntegral w :: Int
{-# INLINE w2i #-}


-- | Converts an int to word limiting the int's value to the 0-255 range
-- before converting it to a Word8 instance
i2w :: Int -> Word8
i2w i = fromIntegral $ clamp 0 255 i :: Word8
{-# INLINE i2w #-}

--------------------------------------------------------------------------------

-- | "Clamps" a given value between an allowed minimum and maximum
clamp :: 
  (Ord a)
    => a -- ^ The minimum clamp value
    -> a -- ^ The maximum clamp value
    -> a -- ^ The value to clamp
    -> a
clamp nMin nMax n = min (max n nMin) nMax
{-# INLINE clamp #-}


-- | Given some number n, this function will convert it to a number n' in [0,1]
unitRange ::
  (Ord a, Floating a, RealFrac a)
  => a
  -> a
unitRange n 
  | n >= 0    = n - (fromIntegral $ floor n)
  | otherwise = -(n - (fromIntegral $ ceiling n))

-- | Given some number n, this function will convert it to a number n' in [-1,1]
negUnitRange ::
  (Ord a, Floating a, RealFrac a)
  => a
  -> a
negUnitRange n 
  | n >= 0    = n - (fromIntegral $ floor n)
  | otherwise = n - (fromIntegral $ ceiling n)

--------------------------------------------------------------------------------

-- | Finds the median of 3 values
median :: 
  (Ord a)
    => a -- ^ Value 1
    -> a -- ^ Value 2
    -> a -- ^ Value 3
    -> a
median x y z = max z $ min x y 
{-# INLINE median #-}


-- | Linear interpolation between two values
-- Adapted from http://paulbourke.net/miscellaneous/interpolation/
lerp :: 
  (Ord a, Floating a)
    => (a, a) -- ^ (min,max) values 
    -> a      -- ^ mu, a value between 0 and 1
    -> a
lerp (x0,x1) mu = x0 * (1.0 - mu) + x1 * mu
{-# INLINE lerp #-}


-- | Performs inverse linear interpolation between a values in an 
-- existing range (,a,b) to a new range (a',b')
ilerp :: 
  (Ord a, Fractional a)
    => a     -- ^ The value to interpolate
    -> (a,a) -- ^ (OldMin,OldMax)
    -> (a,a) -- ^ (NewMin,NewMax)
    -> a
ilerp value (vMin,vMax) (vMin',vMax') =
  (((value - vMin) * range') / range) + vMin'
  where
    range  = vMax - vMin
    range' = vMax' - vMin'
{-# INLINE ilerp #-}


-- | Cosine interpolation between two values
-- Adapted from http://paulbourke.net/miscellaneous/interpolation/
cerp :: 
  (Ord a, Floating a)
    => (a, a) -- ^ (min,max) values 
    -> a      -- ^ mu, a value between 0 and 1
    -> a
cerp (x0,x1) mu = x0 * (1.0 - mu') + x1 * mu'
  where
    mu' = (1.0 - cos (mu * 0.5 * pi)) / 2
{-# INLINE cerp #-}


-- | Like cerp, but mu values are restricted to the range [0,1]
cerp' :: 
  (Ord a, Floating a)
    => (a, a) -- ^ (min,max) values 
    -> a      -- ^ mu, a value between 0 and 1
    -> a
cerp' (x0,x1) mu = x0 * (1.0 - mu') + x1 * mu'
  where
    mu' = clamp 0.0 1.0 $ (1.0 - cos (mu * 0.5 * pi)) / 2
{-# INLINE cerp' #-}

--------------------------------------------------------------------------------

-- | Converts an angle given in degrees to an angle in radians
deg2rad :: 
  (Num a, Floating a)
     => a -- ^ The degree value to convert 
     -> a
deg2rad = (*) radian
{-# INLINE deg2rad #-}


-- | Calculates the disciminant of a quadratic equation
discriminant :: 
  (Num a, Eq a, Floating a)
    => a -- ^ The "a" part of the quadratic equation
    -> a -- ^ The "b" part of the quadratic equation
    -> a -- ^ The "c" part of the quadratic equation
    -> a
discriminant a b c = b' - ac'
  where
    b'  = if b == 0 then 0 else b * b
    ac' = if a == 0 || c == 0 then 0 else 4 * a * c
{-# INLINE discriminant #-}
