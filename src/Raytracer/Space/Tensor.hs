{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Space.Tensor 
  (
   R3(..)
  ,PointR3(..)
  ,VectorR3(..)
  ,AffineMatrix
  ,AffineTransformable(..)
  ,AffineState
  ,TransformMatrices
  ,vec3
  ,mag
  ,unit
  ,cross
  ,point3
  ,dist
  ,dist2
  ,direction
  ,(/->)
  ,direction'
  ,(/=>)
  ,displace
  ,(|->)
  ,dotpv
  ,zeroPoint
  ,zeroVec
  ,average
  ,barycenter
  ,emptyTransforms
  ,stateTransform
  ,makeTransforms
  ,applyTransforms
  ,(<!~)
  ,(~!>)
  ,mBase
  ,mInv
  ,mInvT
  ,affineId
  ,newAffine
  ,withAffine
  ,affineInvert
  ,affineTranspose
  ,affineTranslate
  ,affineRotate
  ,affineScale
  ,invert
  ,transpose
  ,translate
  ,translateUniform
  ,translateX
  ,translateY
  ,translateZ
  ,rotate
  ,rotateUniform
  ,rotateX
  ,rotateY
  ,rotateZ
  ,scale,scaleUniform
  ,scaleX
  ,scaleY
  ,scaleZ
  )
where
import Data.Typeable 
  (
   Typeable
  )
import Text.Printf 
  (
   printf
  )
import Control.Lens 
  (
   Lens'
  ,lens
  )
import Test.QuickCheck.Arbitrary 
  (
   Arbitrary
  ,arbitrary
  )
import Control.Monad 
  (
   liftM3
  )
import Control.Monad.State 
  (
   State
  ,execState
  ,modify
  )
import Raytracer.Utils.Math 
  (
   deg2rad
  )
import Raytracer.Utils.Math.Matrix 
  (
   mult33by31
  ,inverse33
  )

--------------------------------------------------------------------------------

-- | x,y, and z component access for vector & point in the R^3 space
class Num a => R3 a b | b -> a where

  getX   :: b -> a

  getY   :: b -> a

  getZ   :: b -> a

  setX   :: b -> a -> b

  setY   :: b -> a -> b

  setZ   :: b -> a -> b

  (*.)   :: b -> a -> b

  (/.)   :: b -> a -> b

  (|.)   :: b -> b -> a

--------------------------------------------------------------------------------

-- | Averages a list of R3 instances together, returning the averaged
-- instance
average :: (Monoid a, R3 Double a) => [a] -> a
average xs = (mconcat xs) /. (fromIntegral (length xs) :: Double)

--------------------------------------------------------------------------------

-- | VectorR3 (c)
-- > Basic 3 dimensional world-vector definition with x,y,z components
newtype VectorR3 = VectorR3 (Double,Double,Double) 
  deriving (Eq,Typeable)


-- | Zero vector
zeroVec :: VectorR3 
zeroVec = VectorR3 (0.0,0.0,0.0)


instance R3 Double VectorR3 where

  getX (VectorR3 (x,_,_))    = x

  getY (VectorR3 (_,y,_))    = y

  getZ (VectorR3 (_,_,z))    = z

  setX (VectorR3 (_,y,z)) x' = VectorR3 (x',y,z)

  setY (VectorR3 (x,_,z)) y' = VectorR3 (x,y',z)

  setZ (VectorR3 (x,y,_)) z' = VectorR3 (x,y,z')

  (*.) (VectorR3 (x,y,z)) s  = VectorR3 (x * s,y * s,z * s)
  {-# INLINE (*.) #-}

  (/.) (VectorR3 (x,y,z)) s  = VectorR3 (x / s,y / s,z / s)
  {-# INLINE (/.) #-}

  (|.) (VectorR3(x1,y1,z1)) (VectorR3 (x2,y2,z2)) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  {-# INLINE (|.) #-}

--------------------------------------------------------------------------------

instance Num VectorR3 where

  (+) (VectorR3 (x1,y1,z1)) (VectorR3 (x2,y2,z2)) = VectorR3 (x1 + x2,y1 + y2,z1 + z2)

  (-) (VectorR3 (x1,y1,z1)) (VectorR3 (x2,y2,z2)) = VectorR3 (x1 - x2,y1 - y2,z1 - z2)

  (*) (VectorR3 (x1,y1,z1)) (VectorR3 (x2,y2,z2)) = VectorR3 (x1 * x2,y1 * y2,z1 * z2)
  
  abs (VectorR3 (x,y,z)) = VectorR3 (abs x,abs y,abs z)
  
  signum (VectorR3 (x,y,z)) = VectorR3 (signum x,signum y,signum z)
  
  negate (VectorR3 (x,y,z)) = VectorR3 (negate x,negate y,negate z)
  
  fromInteger _  = error "fromInteger not supported for vectors"


instance Fractional VectorR3 where

  (/) (VectorR3(x1,y1,z1)) (VectorR3 (x2,y2,z2)) = VectorR3 (x1 / x2,y1 / y2,z1 / z2)
  
  recip (VectorR3 (x,y,z)) = VectorR3 (recip x,recip y,recip z)
  
  fromRational _ = error "fromRational not supported for vectors"


instance Monoid VectorR3 where

  mempty        = VectorR3 (0.0,0.0,0.0)

  mappend v1 v2 = v1 + v2


instance Bounded VectorR3 where

  minBound = vec3 (-10e4) (-10e4) (-10e4)

  maxBound = vec3 10e4 10e4 10e4


instance Show VectorR3 where

  show (VectorR3 (x,y,z)) = printf "<|%.2f %.2f %.2f|>" x y z


instance Arbitrary VectorR3 where

  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ unit $ vec3 x y z


-- | Creates a new vector instance in R^3 space
vec3 ::    Double   -- ^ x component of the vector
        -> Double   -- ^ y component of the vector
        -> Double   -- ^ z component of the vector
        -> VectorR3
vec3 x y z = VectorR3 (x,y,z)


-- | Calculates the magnitude of a vector
mag ::    VectorR3 -- ^ The vector to find the magnitude (length) of
       -> Double
mag (VectorR3 (x,y,z)) = sqrt $ x' + y' + z'
  where
    x' | x == 0    = 0
       | x == 1    = x 
       | otherwise = x * x
    y' | y == 0    = 0 
       | y == 1    = y 
       | otherwise = y * y
    z' | z == 0    = 0
       | z == 1    = z 
       | otherwise = z * z
{-# INLINE mag #-}


-- | Calculates the norm of the vector, returning a vector of magnitude = 1
unit ::    VectorR3 -- ^ The vector to normalize
        -> VectorR3
unit v = v /. mag v
{-# INLINE unit #-}


-- | Calculates the cross product between two vectors
cross ::    VectorR3 -- ^ Vector "A"
         -> VectorR3 -- ^ Vector "A"
         -> VectorR3
cross (VectorR3(x1,y1,z1)) (VectorR3 (x2,y2,z2)) = VectorR3 (x,y,z)
  where
    x = (y1 * z2) - (z1 * y2)
    y = (z1 * x2) - (x1 * z2)
    z = (x1 * y2) - (y1 * x2)
{-# INLINE cross #-}

--------------------------------------------------------------------------------

-- | PointR3
-- > Basic 3 dimensional world-point definition, stored in column-major form
newtype PointR3 = PointR3 (Double,Double,Double) 
  deriving (Eq,Typeable)


-- | Zero point
zeroPoint :: PointR3 
zeroPoint = PointR3 (0.0,0.0,0.0)


instance R3 Double PointR3 where

  getX (PointR3 (x,_,_))   = x

  getY (PointR3 (_,y,_))   = y

  getZ (PointR3 (_,_,z))   = z

  setX (PointR3 (_,y,z)) x' = PointR3 (x',y,z)

  setY (PointR3 (x,_,z)) y' = PointR3 (x,y',z)

  setZ (PointR3 (x,y,_)) z' = PointR3 (x,y,z')

  (*.) (PointR3 (x,y,z)) s = PointR3 (x * s,y * s,z * s)
  {-# INLINE (*.) #-}

  (/.) (PointR3 (x,y,z)) s = PointR3 (x / s,y / s,z / s)
  {-# INLINE (/.) #-}

  (|.) (PointR3(x1,y1,z1)) (PointR3 (x2,y2,z2)) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  {-# INLINE (|.) #-}


instance  Num PointR3 where

  (+) (PointR3 (x1,y1,z1)) (PointR3 (x2,y2,z2)) = PointR3 (x1 + x2,y1 + y2,z1 + z2)

  (-) (PointR3 (x1,y1,z1)) (PointR3 (x2,y2,z2)) = PointR3 (x1 - x2,y1 - y2,z1 - z2)

  (*) (PointR3 (x1,y1,z1)) (PointR3 (x2,y2,z2)) = PointR3 (x1 * x2,y1 * y2,z1 * z2)
  
  abs (PointR3 (x,y,z)) = PointR3 (abs x,abs y,abs z)
  
  signum (PointR3 (x,y,z)) = PointR3 (signum x,signum y,signum z)
  
  negate (PointR3 (x,y,z)) = PointR3 (negate x,negate y,negate z)
  
  fromInteger _  = error "fromInteger not supported for points"


instance Fractional PointR3 where

  (/) (PointR3 (x1,y1,z1)) (PointR3 (x2,y2,z2)) = PointR3 (x1 / x2,y1 / y2,z1 / z2)
  
  recip (PointR3 (x,y,z)) = PointR3 (recip x,recip y,recip z)
  
  fromRational _ = error "fromRational not supported for points"


instance Monoid PointR3 where

  mempty        = PointR3 (0.0,0.0,0.0)

  mappend v1 v2 = v1 + v2


instance Bounded PointR3 where

  minBound = point3 (-10e4) (-10e4) (-10e4)

  maxBound = point3 10e4 10e4 10e4


instance Show PointR3 where

  show (PointR3 (x,y,z)) = printf "[|%.2f %.2f %.2f|]" x y z


instance Arbitrary PointR3 where

  arbitrary = liftM3 point3 arbitrary arbitrary arbitrary


-- | Creates a new point instance in R^3 space
point3 ::   Double -- ^ The x component of the point
         -> Double -- ^ The y component of the point
         -> Double -- ^ The z component of the point
         -> PointR3
point3 x y z = PointR3 (x,y,z)


-- | Calculates the Euclidean distance between 2 points
dist ::    PointR3 -- ^ Point "A"
        -> PointR3 -- ^ Point "B"
        -> Double
dist (PointR3 (x1,y1,z1)) (PointR3 (x2,y2,z2)) = 
  sqrt $ (x' * x') + (y' * y') + (z' * z')
  where
    x' = x2 - x1
    y' = y2 - y1
    z' = z2 - z1
{-# INLINE dist #-}


-- | Calculates the squared distance between 2 points
dist2 ::   PointR3 -- ^ Point "A"
        -> PointR3 -- ^ Point "B"
        -> Double
dist2 (PointR3 (x1,y1,z1)) (PointR3 (x2,y2,z2)) = (x' * x') + (y' * y') + (z' * z')
  where
    x' = x2 - x1
    y' = y2 - y1
    z' = z2 - z1


-- | Computes the direction between 2 points, returning a unit vector
direction ::    PointR3  -- ^ The "from" point
             -> PointR3  -- ^ The "to" point
             -> VectorR3
direction (PointR3 (fromX,fromY,fromZ)) (PointR3 (toX,toY,toZ)) =
  unit $ VectorR3 (fromX - toX,fromY - toY,fromZ - toZ)
{-# INLINE direction #-}


-- | Like direction, but maintains the length of the resulting vector
direction' ::   PointR3  -- ^ The "from" point
             -> PointR3  -- ^ The "to" point
             -> VectorR3
direction' (PointR3 (fromX,fromY,fromZ)) (PointR3 (toX,toY,toZ)) =
  VectorR3 (fromX - toX,fromY - toY,fromZ - toZ)
{-# INLINE direction' #-}


-- | The infix operator equivalent of the direction function
(/->) ::    PointR3  -- ^ The "from" point
         -> PointR3  -- ^ The "to" point
         -> VectorR3
(/->) = direction


-- | The infix operator equivalent of the direction' function
(/=>) ::    PointR3  -- ^ The "from" point
         -> PointR3  -- ^ The "to" point
         -> VectorR3
(/=>) = direction'


-- | Displaces a point by a vector, yielding a new point
displace ::    PointR3  -- ^ The starting point
            -> VectorR3 -- ^ The direction vector to translate the point by
            -> PointR3
displace (PointR3 (px,py,pz)) (VectorR3 (vx,vy,vz)) = PointR3 (px + vx,py + vy,pz + vz)
{-# INLINE displace #-}


-- | The infix operator equivalent of the `displace' function
(|->) ::    PointR3  -- ^ The starting point
         -> VectorR3 -- ^ The direction vector to translate the point by
         -> PointR3
(|->) = displace


-- | (Pseudo) dot product between a PointR3nd VectorR3 instance. Again, like
-- (|.), although it doesn't really make sense to compute the dot produce
-- of a vector and point, in practice, it is conventient at times to do so.
dotpv ::    PointR3 
         -> VectorR3 
         -> Double
dotpv (PointR3 (px,py,pz)) (VectorR3 (vx,vy,vz)) = (px * vx) + (py * vy) + (pz * vz)
{-# INLINE dotpv #-}

-- For 3 points, returns the barycenter (u,v,w), where u + v + w = 1.0
-- Adapted from http://gamedev.stackexchange.com/a/23745
barycenter :: 
     PointR3                   -- ^ The point, p, the coordinates are computed for
  -> (PointR3,PointR3,PointR3) -- ^ The 3 vertices that define the extent of the triangle
  -> (Double,Double,Double)
barycenter p (v0,v1,v2) = (u,v,w)
  where 
    e0    = v1 /=> v0
    e1    = v2 /=> v0 
    e2    = p /=> v0
    d00   = e0 |. e0
    d01   = e0 |. e1
    d11   = e1 |. e1
    d20   = e2 |. e0
    d21   = e2 |. e1
    denom = (d00 * d11) - (d01 * d01)
    v     = ((d11 * d20) - (d01 * d21)) / denom
    w     = ((d00 * d21) - (d01 * d20)) / denom
    u     = 1.0 - v - w
{-# INLINE barycenter #-}

--------------------------------------------------------------------------------

-- | Affine matrix type used to encode a series of affine transformations
-- 4x4 Affine transformation matrix
--newtype AffineMatrix = A (Matrix Double) deriving (Eq,Typeable)


newtype AffineMatrix = A (Double,Double,Double,Double
                         ,Double,Double,Double,Double
                         ,Double,Double,Double,Double
                         ,Double,Double,Double,Double)
  deriving (Typeable)


affineId :: AffineMatrix
affineId = A (1.0, 0.0, 0.0, 0.0
             ,0.0, 1.0, 0.0, 0.0
             ,0.0 ,0.0, 1.0, 0.0
             ,0.0, 0.0, 0.0, 1.0)


instance Show AffineMatrix where

  show (A (m11,m12,m13,m14
          ,m21,m22,m23,m24
          ,m31,m32,m33,m34
          ,m41,m42,m43,m44)) = 
    printf ("[%2.f %2.f %2.f %2.f\n" ++ 
            " %2.f %2.f %2.f %2.f\n" ++
            " %2.f %2.f %2.f %2.f\n" ++
            " %2.f %2.f %2.f %2.f]\n") 
      m11 m12 m13 m14 
      m21 m22 m23 m24 
      m31 m32 m33 m34 
      m41 m42 m43 m44

--------------------------------------------------------------------------------

-- | Type representing the intermediate results of a sequence 
-- of affine transformations for use in a monadic context
type AffineState = State AffineMatrix ()

--------------------------------------------------------------------------------

-- The whole suite of affine matrics:
-- * The original affine matrix m
-- * The inverse of m, mi
-- * The transpose of the inverse of m, mit
type TransformMatrices = (AffineMatrix,AffineMatrix,AffineMatrix)


-- | Creates a new triple of matrix transforms:
--   (affineId, affineId, affineId)
emptyTransforms :: TransformMatrices
emptyTransforms = (affineId,affineId,affineId)


-- | Applies the given affine state to the triple return from emptyTransforms,
-- yielding a triple of the form:
--   (matrix, inv(matrix), transpose(inv(matrix)))
stateTransform ::    
     AffineState       -- ^ The affine state to apply
  -> TransformMatrices
stateTransform st = applyTransforms affineId $ newAffine st


-- Given an affine matrix m, this function generates a triple of matrix 
-- transformations:
--   (m, inv(m), transpose(inv(m)))
makeTransforms ::    
     AffineMatrix
  -> TransformMatrices
makeTransforms m = (m,mi,mit)
  where
    mi  = affineInvert m
    mit = affineTranspose mi


-- | Given a transformation affine matrix t and an initial affine matrix m,
-- this function applies t to m, yielding m' which is used to generate
-- a new triple:
applyTransforms ::    
     AffineMatrix      -- ^ The transformation affine matrix
  -> AffineMatrix      -- ^ The initial affine matrix
  -> TransformMatrices
applyTransforms t m = makeTransforms $ m * t


-- | Symbolic operator alias for applyTransforms
(<!~) ::    
     AffineMatrix      -- ^ The transformation affine matrix
  -> AffineMatrix      -- ^ The initial affine matrix
  -> TransformMatrices
(<!~) = applyTransforms


-- | (<!~) with flipped operands
(~!>) ::   
     AffineMatrix      -- ^ The initial affine matrix
  -> AffineMatrix      -- ^ The transformation affine matrix
  -> TransformMatrices
(~!>) = flip applyTransforms


-- | Lens for base affine matrix, m in the TransformMatrices tuple
mBase :: Lens' TransformMatrices AffineMatrix
mBase = lens getter setter
  where
    getter (m,_,_)       = m
    setter (_,mi,mit) m' = (m',mi,mit)


-- | Lens for inverse affine matrix in the TransformMatrices tuple
mInv :: Lens' TransformMatrices AffineMatrix
mInv = lens getter setter
  where
    getter (_,mi,_)      = mi
    setter (m,_,mit) mi' = (m,mi',mit)


-- | Lens for inverse transpose affine matrix in the TransformMatrices tuple
mInvT :: Lens' TransformMatrices AffineMatrix
mInvT = lens getter setter
  where
    getter (_,_,mit)     = mit
    setter (m,mi,_) mit' = (m,mi,mit')

--------------------------------------------------------------------------------

-- | To be implemented by types that can be affine transformed
class AffineTransformable a where

  -- | Applies the transformations encoded in the given affine matrix to
  -- the supplied object
  (<!-)  ::    
       a             -- ^ The object to transform
    -> AffineMatrix  -- ^ The transformation matrix
    -> a

  -- | "Flipped" version of the (-!>) operator that takes an AffineState 
  -- instance first, then the object to transform
  (-!>)  ::    
       AffineMatrix -- ^ The transformation matrix
    -> a            -- ^ The object to transform
    -> a
  (-!>) = flip (<!-)

  -- | Basically, this is the "do" notation equivalent of the (<!-) operator.
  (<!=) ::    
       a           -- ^ The object to transform
    -> AffineState -- ^ The intermediate transformation state
    -> a
  (<!=) obj st = obj <!- (execState st $ affineId)

  -- | "Flipped" version of the (=!>) operator that takes an AffineState 
  -- instance first, then the object to transform
  (=!>) ::    
       AffineState -- ^ The intermediate transformation state
    -> a           -- ^ The object to transform
    -> a
  (=!>) = flip (<!=)

  -- | Like the (<!=), but allows for a user-defined affine matrix to be passed
  -- in, rather than using the affine idenity matrix, affineId, as the starting
  -- point
  withTransform ::    
       a
    -> AffineMatrix
    -> AffineState
    -> a
  withTransform obj m st = obj <!- (execState st $ m)

--------------------------------------------------------------------------------

instance Monoid AffineMatrix where

  mempty  = affineId

  mappend = (*)


instance Num AffineMatrix where

  (+) (A (m11,m12,m13,m14   
         ,m21,m22,m23,m24
         ,m31,m32,m33,m34
         ,m41,m42,m43,m44))
      (A (n11,n12,n13,n14   
         ,n21,n22,n23,n24
         ,n31,n32,n33,n34
         ,n41,n42,n43,n44)) =
      A (m11 + n11,m12 + n12,m13 + n13,m14 + n14
        ,m21 + n21,m22 + n22,m23 + n23,m24 + n24
        ,m31 + n31,m32 + n32,m33 + n33,m34 + n34
        ,m41 + n41,m42 + n42,m43 + n43,m44 + n44)

  (-) (A (m11,m12,m13,m14   
         ,m21,m22,m23,m24
         ,m31,m32,m33,m34
         ,m41,m42,m43,m44))
      (A (n11,n12,n13,n14   
         ,n21,n22,n23,n24
         ,n31,n32,n33,n34
         ,n41,n42,n43,n44)) =
      A (m11 - n11,m12 - n12,m13 - n13,m14 - n14
        ,m21 - n21,m22 - n22,m23 - n23,m24 - n24
        ,m31 - n31,m32 - n32,m33 - n33,m34 - n34
        ,m41 - n41,m42 - n42,m43 - n43,m44 - n44)

  (*) (A (m11,m12,m13,m14   
         ,m21,m22,m23,m24
         ,m31,m32,m33,m34
         ,m41,m42,m43,m44))
      (A (n11,n12,n13,n14   
         ,n21,n22,n23,n24
         ,n31,n32,n33,n34
         ,n41,n42,n43,n44)) =
    A ((m11 * n11) + (m12 * n21) + (m13 * n31) + (m14 * n41)  -- 1,1
      ,(m11 * n12) + (m12 * n22) + (m13 * n32) + (m14 * n42)  -- 1,2
      ,(m11 * n13) + (m12 * n23) + (m13 * n33) + (m14 * n43)  -- 1,3
      ,(m11 * n14) + (m12 * n24) + (m13 * n34) + (m14 * n44)  -- 1,4
      ,(m21 * n11) + (m22 * n21) + (m23 * n31) + (m24 * n41)  -- 2,1
      ,(m21 * n12) + (m22 * n22) + (m23 * n32) + (m24 * n42)  -- 2,2
      ,(m21 * n13) + (m22 * n23) + (m23 * n33) + (m24 * n43)  -- 2,3
      ,(m21 * n14) + (m22 * n24) + (m23 * n34) + (m24 * n44)  -- 2,4
      ,(m31 * n11) + (m32 * n21) + (m33 * n31) + (m34 * n41)  -- 3,1
      ,(m31 * n12) + (m32 * n22) + (m33 * n32) + (m34 * n42)  -- 3,2
      ,(m31 * n13) + (m32 * n23) + (m33 * n33) + (m34 * n43)  -- 3,3
      ,(m31 * n14) + (m32 * n24) + (m33 * n34) + (m34 * n44)  -- 3,4
      ,(m41 * n11) + (m42 * n21) + (m43 * n31) + (m44 * n41)  -- 4,1
      ,(m41 * n12) + (m42 * n22) + (m43 * n32) + (m44 * n42)  -- 4,2
      ,(m41 * n13) + (m42 * n23) + (m43 * n33) + (m44 * n43)  -- 4,3
      ,(m41 * n14) + (m42 * n24) + (m43 * n34) + (m44 * n44)) -- 4,3
  
  abs (A (m11,m12,m13,m14
         ,m21,m22,m23,m24
         ,m31,m32,m33,m34
         ,m41,m42,m43,m44)) = 
    A (abs m11,abs m12,abs m13,abs m14
      ,abs m21,abs m22,abs m23,abs m24
      ,abs m31,abs m32,abs m33,abs m34
      ,abs m41,abs m42,abs m43,abs m44)
  
  signum (A (m11,m12,m13,m14
            ,m21,m22,m23,m24
            ,m31,m32,m33,m34
            ,m41,m42,m43,m44)) = 
    A (signum m11,signum m12,signum m13,signum m14
      ,signum m21,signum m22,signum m23,signum m24
      ,signum m31,signum m32,signum m33,signum m34
      ,signum m41,signum m42,signum m43,signum m44)

  negate (A (m11,m12,m13,m14
            ,m21,m22,m23,m24
            ,m31,m32,m33,m34
            ,m41,m42,m43,m44)) = 
    A (negate m11,negate m12,negate m13,negate m14
      ,negate m21,negate m22,negate m23,negate m24
      ,negate m31,negate m32,negate m33,negate m34
      ,negate m41,negate m42,negate m43,negate m44)
  
  fromInteger _ = error "fromInteger not supported for matrices"


-- | Applies an affine translation operation to the supplied affine matrix
affineTranslate ::    
     Double       -- ^ The x translation component
  -> Double       -- ^ The x translation component
  -> Double       -- ^ The x translation component
  -> AffineMatrix -- ^ The affine matrix to translate
  -> AffineMatrix
affineTranslate 0.0 0.0 0.0 m = m
affineTranslate tx ty tz (A (m11,m12,m13,m14
                            ,m21,m22,m23,m24
                            ,m31,m32,m33,m34
                            ,m41,m42,m43,m44)) =
  A (m11,m12,m13,m14 + tx
    ,m21,m22,m23,m24 + ty
    ,m31,m32,m33,m34 + tz
    ,m41,m42,m43,m44)


-- | Applies an affine rotation operation to the supplied affine matrix
affineRotate ::    
     Double       -- ^ The x rotation component in degrees
  -> Double       -- ^ The y rotation component in degrees
  -> Double       -- ^ The z rotation component in degrees
  -> AffineMatrix -- ^ The affine matrix to rotation
  -> AffineMatrix
affineRotate 0.0 0.0 0.0 m = m
affineRotate rx ry rz m    =
  let rxM   = if rx /= 0.0 then Just $ computeX $ deg2rad rx else Nothing
      ryM   = if ry /= 0.0 then Just $ computeY $ deg2rad ry else Nothing
      rzM   = if rz /= 0.0 then Just $ computeZ $ deg2rad rz else Nothing
  in  case (rxM ^* ryM ^* rzM ^* (Just m)) of
        Just m' -> m'
        Nothing -> m
  where
    computeX x = A (1.0,   0.0,   0.0, 0.0
                   ,0.0, cos x, sin x, 0.0
                   ,0.0,-sin x, cos x, 0.0
                   ,0.0,   0.0,   0.0, 1.0)

    computeY y = A (cos y, 0.0, -sin y, 0.0
                   ,0.0,   1.0,    0.0, 0.0
                   ,sin y, 0.0,  cos y, 0.0
                   ,0.0,   0.0,    0.0, 1.0)

    computeZ z = A ( cos z, sin z, 0.0, 0.0
                   ,-sin z, cos z, 0.0, 0.0
                   ,0.0,   0.0,    1.0, 0.0
                   ,0.0,   0.0,    0.0, 1.0)

    (^*) (Just m) (Just n) = Just (m * n)
    (^*) (Just m) Nothing  = Just m
    (^*) _ x               = x


--affineRotate ::    
--     Double       -- ^ The x rotation component in degrees
--  -> Double       -- ^ The y rotation component in degrees
--  -> Double       -- ^ The z rotation component in degrees
--  -> AffineMatrix -- ^ The affine matrix to rotation
--  -> AffineMatrix
--affineRotate rx ry rz m =
--  A (m11, m12, m13, m14
--    ,m21, m22, m23, m24
--    ,m31, m32, m33, m34
--    ,m41, m42, m43, m44) * m
--  where
--    x   = deg2rad rx
--    y   = deg2rad ry 
--    z   = deg2rad rz
--    m11 = (cos y) * (cos z)
--    m12 = (cos y) * (sin z)
--    m13 = -(sin y)
--    m14 = 0
--    m21 = ((cos z) * (sin x)) + ((cos x) * (-(sin z)))
--    m22 = ((sin x) * (sin z)) + ((cos z) * (cos x))
--    m23 = (sin x) * (cos y)
--    m24 = 0
--    m31 = ((cos x) * (sin y) * (cos z)) + (-(sin x) * (-(sin z)))
--    m32 = ((sin z) * (cos x) * (sin y)) + ((cos z) * (-(sin x)))
--    m33 = ((cos x) * (cos y))
--    m34 = 0
--    m41 = 0
--    m42 = 0
--    m43 = 0
--    m44 = 1


-- | Applies an affine scaling operation to the supplied affine matrix
affineScale ::   
     Double       -- ^ x scale component to scale by
  -> Double       -- ^ y scale component to scale by
  -> Double       -- ^ z scale component to scale by
  -> AffineMatrix -- ^ The affine matrix to scale
  -> AffineMatrix
affineScale 1.0 1.0 1.0 m = m
affineScale sx sy sz (A (m11,m12,m13,m14
                        ,m21,m22,m23,m24
                        ,m31,m32,m33,m34
                        ,m41,m42,m43,m44)) =
  A (m11 * sx,m12,m13,m14
    ,m21, m22 * sy,m23,m24
    ,m31,m32,m33 * sz,m34
    ,m41,m42,m43,m44)


-- | Inverts the given affine matrix
affineInvert ::    
     AffineMatrix -- ^ The affine matrix to invert
  -> AffineMatrix
affineInvert (A (m11,m12,m13,b1
                ,m21,m22,m23,b2
                ,m31,m32,m33,b3
                ,0.0,0.0,0.0,1.0)) = 
  A (m11',m12',m13', mb1
    ,m21',m22',m23', mb2
    ,m31',m32',m33', mb3
    , 0.0, 0.0, 0.0, 1.0)
  where
    (m11',m12',m13',m21',m22',m23',m31',m32',m33') =
      inverse33 (m11,m12,m13 ,m21,m22,m23 ,m31,m32,m33)
    (mb1,mb2,mb3) = 
        (-m11',-m12',-m13'
        ,-m21',-m22',-m23'
        ,-m31',-m32',-m33') `mult33by31` (b1,b2,b3)
affineInvert _ = error "Not affine!"


-- | Transposes the given affine matrix
affineTranspose ::    
     AffineMatrix -- ^ The affine matrix to transpose
  -> AffineMatrix
affineTranspose (A (m11,m12,m13,m14
                   ,m21,m22,m23,m24
                   ,m31,m32,m33,m34
                   ,m41,m42,m43,m44)) = 
  A (m11,m21,m31,m41
    ,m12,m22,m32,m42
    ,m13,m23,m33,m43
    ,m14,m24,m34,m44)


-- | Apply an affine transformation to a vector
instance AffineTransformable VectorR3 where

  (<!-) (VectorR3 (vx,vy,vz)) (A (m11,m12,m13,_
                            ,m21,m22,m23,_
                            ,m31,m32,m33,_
                            ,  _,  _,  _,_)) = 
    VectorR3 (x',y',z')
    where
      x' = (vx * m11) + (vy * m12) + (vz * m13)
      y' = (vx * m21) + (vy * m22) + (vz * m23)
      z' = (vx * m31) + (vy * m32) + (vz * m33)


-- | Apply an affine transformation to a point
instance AffineTransformable PointR3 where

  (<!-) (PointR3 (px,py,pz)) (A (m11,m12,m13,m14
                            ,m21,m22,m23,m24
                            ,m31,m32,m33,m34
                            ,m41,m42,m43,m44))
    | w' /= 0 && w' /= 1 = PointR3 (x' / w',y' / w',z' / w')
    | otherwise          = PointR3 (x',y',z')
    where
      x' = (px * m11) + (py * m12) + (pz * m13) + m14
      y' = (px * m21) + (py * m22) + (pz * m23) + m24
      z' = (px * m31) + (py * m32) + (pz * m33) + m34
      w' = (px * m41) + (py * m42) + (pz * m43) + m44

--------------------------------------------------------------------------------

newAffine ::    AffineState 
             -> AffineMatrix
newAffine st = execState st $ affineId


withAffine ::    AffineMatrix
              -> AffineState
              -> AffineMatrix
withAffine m st = execState st m


invert :: AffineState
invert = modify $ \m -> affineInvert m


transpose :: AffineState
transpose = modify $ \m -> affineTranspose m


translate ::    Double
             -> Double
             -> Double
             -> AffineState
translate x y z = modify $ \m -> affineTranslate x y z m


-- |
translateUniform ::    Double 
                    -> AffineState
translateUniform by = modify $ \m -> affineTranslate by by by m


translateX ::    Double
              -> AffineState
translateX x = modify $ \m -> affineTranslate x 0 0 m


translateY ::    Double
              -> AffineState
translateY y = modify $ \m -> affineTranslate 0 y 0 m


translateZ ::    Double
              -> AffineState
translateZ z = modify $ \m -> affineTranslate 0 0 z m


rotate ::    Double
          -> Double
          -> Double
          -> AffineState
rotate x y z = modify $ \m -> affineRotate x y z m


rotateUniform ::    Double 
                 -> AffineState
rotateUniform by = modify $ \m -> affineRotate by by by m


rotateX ::    Double
           -> AffineState
rotateX x = modify $ \m -> affineRotate x 0 0 m


rotateY ::    Double
           -> AffineState
rotateY y = modify $ \m -> affineRotate 0 y 0 m


rotateZ ::    Double
           -> AffineState
rotateZ z = modify $ \m -> affineRotate 0 0 z m


scale ::    Double
         -> Double
         -> Double
         -> AffineState
scale x y z = modify $ \m -> affineScale x y z m


scaleUniform ::    Double
                -> AffineState
scaleUniform by = modify $ \m -> affineScale by by by m


scaleX ::    Double
          -> AffineState
scaleX x = modify $ \m -> affineScale x 1 1 m


scaleY ::    Double 
          -> AffineState
scaleY y = modify $ \m -> affineScale 1 y 1 m


scaleZ ::    Double
          -> AffineState
scaleZ z = modify $ \m -> affineScale 1 1 z m
