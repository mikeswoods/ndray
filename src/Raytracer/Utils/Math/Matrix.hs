{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Raytracer.Utils.Math.Matrix (mult33by31, det33, inverse33)
where

--------------------------------------------------------------------------------

-- | Multiplies a 3x3 matrix by a 3x1 vector
mult33by31 ::
     (Double,Double,Double
     ,Double,Double,Double
     ,Double,Double,Double)
  -> (Double,Double,Double)
  -> (Double,Double,Double)
mult33by31 (a11,a12,a13
           ,a21,a22,a23
           ,a31,a32,a33)
           (b1,b2,b3) =
    ((a11 * b1) + (a12 * b2) + (a13 * b3)
    ,(a21 * b1) + (a22 * b2) + (a23 * b3)
    ,(a31 * b1) + (a32 * b2) + (a33* b3))
{-# INLINE mult33by31 #-}


-- | Computes the determinant of a 3x3 matrix 
det33 :: 
     (Double,Double,Double
     ,Double,Double,Double
     ,Double,Double,Double)
  -> Double
det33 (a11,a12,a13
      ,a21,a22,a23
      ,a31,a32,a33) =   
    (a11 * ((a33 * a22) - (a32 * a23)))
  - (a21 * ((a33 * a12) - (a32 * a13)))
  + (a31 * ((a23 * a12) - (a22 * a13)))
{-# INLINE det33 #-}


-- | Computes the inverse of a 3x3 aatrix
inverse33 :: 
     (Double,Double,Double
     ,Double,Double,Double
     ,Double,Double,Double)
  -> (Double,Double,Double
     ,Double,Double,Double
     ,Double,Double,Double)
inverse33 a@(a11,a12,a13
            ,a21,a22,a23
            ,a31,a32,a33) =
  (d * n11,d * n12,d * n13
  ,d * n21,d * n22,d * n23
  ,d * n31,d * n32,d * n33)
  where
    d   = 1.0 / (det33 a)
    n11 = (a33 * a22) - (a32 * a23)
    n12 = -((a33 * a12) - (a32 * a13))
    n13 =  (a23 * a12) - (a22 * a13)
    n21 = -((a33 * a21) - (a31 * a23))
    n22 = (a33 * a11) - (a31 * a13)
    n23 = -((a23 * a11) - (a21 * a13))
    n31 = (a32 * a21) - (a31 * a22)
    n32 = -((a32 * a11) - (a31 * a12))
    n33 = (a22 * a11) - (a21 * a12)
