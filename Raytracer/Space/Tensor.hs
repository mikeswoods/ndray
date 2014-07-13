{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Raytracer.Space.Tensor (R3(..), VectorR3, vec3, mag, unit
  ,cross, PointR3, point3, dist, dist2, direction, (/->), direction', (/=>)
  ,displace, (|->), dotpv
  ,average, barycenter
  ,AffineMatrix, AffineTransformable(..), AffineState, TransformMatrices
  ,emptyTransforms, stateTransform, makeTransforms, applyTransforms,(<!~), (~!>)
  ,mBase, mInv, mInvT
  ,toList, fromList
  ,affineId, newAffine, withAffine, affineInvert
  ,affineTranspose, affineTranslate, affineRotate, affineScale
  ,invert, transpose
  ,translate, translateUniform, translateX, translateY, translateZ
  ,rotate, rotateUniform, rotateX, rotateY, rotateZ
  ,scale,scaleUniform, scaleX, scaleY, scaleZ
  ,runTests)
where
import Data.Maybe (isJust, fromJust)
import Data.Char (isSpace)
import Text.ParserCombinators.ReadP (readP_to_S
  ,skipSpaces, between, many, string, satisfy)
import Text.Printf (printf)
import Test.HUnit hiding (State)
import Numeric.Matrix (Matrix, MatrixElement, inv)
import qualified Numeric.Matrix as M (at, times, fromList, toList
                                     ,diag, transpose, unit, map)
import Control.Lens (Lens', lens)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Control.Monad (liftM3)
import Control.Monad.State (State, execState, modify)
import Data.Monoid (Monoid(..))
import Raytracer.Utils.Math (deg2rad)

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

-- | Internal
parseCoordinateString ::
     String 
  -> String 
  -> Maybe String 
  -> (Double -> Double -> Double -> a) 
  -> ReadS a
parseCoordinateString open close maybeSep f = readP_to_S $ do
  between (string open) (string close) $ do
    x <- getNumber
    checkForSep
    y <- getNumber
    checkForSep
    z <- getNumber
    return $ f x y z
  where
    checkForSep
      | isJust maybeSep = do
        _ <- string $ fromJust maybeSep
        return ()
      | otherwise = return ()
    getNumber = do
      skipSpaces
      n <- many $ satisfy $ not . isSpace
      skipSpaces
      return (read n :: Double)

--------------------------------------------------------------------------------

-- | Averages a list of R3 instances together, returning the averaged
-- instance
average :: (Monoid a, R3 Double a) => [a] -> a
average xs = (mconcat xs) /. (fromIntegral (length xs) :: Double)

--------------------------------------------------------------------------------

-- | VectorR3 (c)
-- > Basic 3 dimensional world-vector definition with x,y,z components
newtype VectorR3 = MkV (Double,Double,Double) deriving (Eq)


instance R3 Double VectorR3 where

  getX (MkV (x,_,_))    = x

  getY (MkV (_,y,_))    = y

  getZ (MkV (_,_,z))    = z

  setX (MkV (_,y,z)) x' = MkV (x',y,z)

  setY (MkV (x,_,z)) y' = MkV (x,y',z)

  setZ (MkV (x,y,_)) z' = MkV (x,y,z')

  (*.) (MkV (x,y,z)) s  = MkV (x * s,y * s,z * s)
  {-# INLINE (*.) #-}

  (/.) (MkV (x,y,z)) s  = MkV (x / s,y / s,z / s)
  {-# INLINE (/.) #-}

  (|.) (MkV(x1,y1,z1)) (MkV (x2,y2,z2)) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  {-# INLINE (|.) #-}

--------------------------------------------------------------------------------

instance Num VectorR3 where

  (+) (MkV(x1,y1,z1)) (MkV (x2,y2,z2)) = MkV (x1 + x2,y1 + y2,z1 + z2)

  (-) (MkV(x1,y1,z1)) (MkV (x2,y2,z2)) = MkV (x1 - x2,y1 - y2,z1 - z2)

  (*) (MkV(x1,y1,z1)) (MkV (x2,y2,z2)) = MkV (x1 * x2,y1 * y2,z1 * z2)
  
  abs (MkV (x,y,z)) = MkV (abs x,abs y,abs z)
  
  signum (MkV (x,y,z)) = MkV (signum x,signum y,signum z)
  
  negate (MkV (x,y,z)) = MkV (negate x,negate y,negate z)
  
  fromInteger _  = error "fromInteger not supported for vectors"


instance Fractional VectorR3 where

  (/) (MkV(x1,y1,z1)) (MkV (x2,y2,z2)) = MkV (x1 / x2,y1 / y2,z1 / z2)
  
  recip (MkV (x,y,z)) = MkV (recip x,recip y,recip z)
  
  fromRational _ = error "fromRational not supported for vectors"


instance Monoid VectorR3 where

  mempty        = MkV (0.0,0.0,0.0)

  mappend v1 v2 = v1 + v2


instance Bounded VectorR3 where

  minBound = vec3 (-10e4) (-10e4) (-10e4)

  maxBound = vec3 10e4 10e4 10e4


instance Show VectorR3 where

  show (MkV (x,y,z)) = printf "<|%.2f %.2f %.2f|>" x y z


instance Read VectorR3 where

  readsPrec _ = parseCoordinateString "<|" "|>" Nothing vec3


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
vec3 x y z = MkV (x,y,z)


-- | Calculates the magnitude of a vector
mag ::    VectorR3 -- ^ The vector to find the magnitude (length) of
       -> Double
mag (MkV (x,y,z)) = sqrt $ x' + y' + z'
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
cross (MkV(x1,y1,z1)) (MkV (x2,y2,z2)) = MkV (x,y,z)
  where
    x = (y1 * z2) - (z1 * y2)
    y = (z1 * x2) - (x1 * z2)
    z = (x1 * y2) - (y1 * x2)
{-# INLINE cross #-}

--------------------------------------------------------------------------------

-- | PointR3
-- > Basic 3 dimensional world-point definition, stored in column-major form
newtype PointR3 = MkP (Double,Double,Double) deriving (Eq)


instance R3 Double PointR3 where

  getX (MkP (x,_,_))   = x

  getY (MkP (_,y,_))   = y

  getZ (MkP (_,_,z))   = z

  setX (MkP (_,y,z)) x' = MkP (x',y,z)

  setY (MkP (x,_,z)) y' = MkP (x,y',z)

  setZ (MkP (x,y,_)) z' = MkP (x,y,z')

  (*.) (MkP (x,y,z)) s = MkP (x * s,y * s,z * s)
  {-# INLINE (*.) #-}

  (/.) (MkP (x,y,z)) s = MkP (x / s,y / s,z / s)
  {-# INLINE (/.) #-}

  (|.) (MkP(x1,y1,z1)) (MkP (x2,y2,z2)) = (x1 * x2) + (y1 * y2) + (z1 * z2)
  {-# INLINE (|.) #-}


instance  Num PointR3 where

  (+) (MkP (x1,y1,z1)) (MkP (x2,y2,z2)) = MkP (x1 + x2,y1 + y2,z1 + z2)

  (-) (MkP (x1,y1,z1)) (MkP (x2,y2,z2)) = MkP (x1 - x2,y1 - y2,z1 - z2)

  (*) (MkP (x1,y1,z1)) (MkP (x2,y2,z2)) = MkP (x1 * x2,y1 * y2,z1 * z2)
  
  abs (MkP (x,y,z)) = MkP (abs x,abs y,abs z)
  
  signum (MkP (x,y,z)) = MkP (signum x,signum y,signum z)
  
  negate (MkP (x,y,z)) = MkP (negate x,negate y,negate z)
  
  fromInteger _  = error "fromInteger not supported for points"


instance Fractional PointR3 where

  (/) (MkP (x1,y1,z1)) (MkP (x2,y2,z2)) = MkP (x1 / x2,y1 / y2,z1 / z2)
  
  recip (MkP (x,y,z)) = MkP (recip x,recip y,recip z)
  
  fromRational _ = error "fromRational not supported for points"


instance Monoid PointR3 where

  mempty        = MkP (0.0,0.0,0.0)

  mappend v1 v2 = v1 + v2


instance Bounded PointR3 where

  minBound = point3 (-10e4) (-10e4) (-10e4)

  maxBound = point3 10e4 10e4 10e4


instance Show PointR3 where

  show (MkP (x,y,z)) = printf "[|%.2f %.2f %.2f|]" x y z


instance Read PointR3 where

  readsPrec _ = parseCoordinateString "[|" "|]" Nothing point3


instance Arbitrary PointR3 where

  arbitrary = liftM3 point3 arbitrary arbitrary arbitrary


-- | Creates a new point instance in R^3 space
point3 ::   Double -- ^ The x component of the point
         -> Double -- ^ The y component of the point
         -> Double -- ^ The z component of the point
         -> PointR3
point3 x y z = MkP (x,y,z)


-- | Calculates the Euclidean distance between 2 points
dist ::    PointR3 -- ^ Point "A"
        -> PointR3 -- ^ Point "B"
        -> Double
dist (MkP (x1,y1,z1)) (MkP (x2,y2,z2)) = 
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
dist2 (MkP (x1,y1,z1)) (MkP (x2,y2,z2)) = (x' * x') + (y' * y') + (z' * z')
  where
    x' = x2 - x1
    y' = y2 - y1
    z' = z2 - z1


-- | Computes the direction between 2 points, returning a unit vector
direction ::    PointR3  -- ^ The "from" point
             -> PointR3  -- ^ The "to" point
             -> VectorR3
direction (MkP (fromX,fromY,fromZ)) (MkP (toX,toY,toZ)) =
  unit $ MkV (fromX - toX,fromY - toY,fromZ - toZ)
{-# INLINE direction #-}


-- | Like direction, but maintains the length of the resulting vector
direction' ::   PointR3  -- ^ The "from" point
             -> PointR3  -- ^ The "to" point
             -> VectorR3
direction' (MkP (fromX,fromY,fromZ)) (MkP (toX,toY,toZ)) =
  MkV (fromX - toX,fromY - toY,fromZ - toZ)
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
displace (MkP (px,py,pz)) (MkV (vx,vy,vz)) = MkP (px + vx,py + vy,pz + vz)
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
dotpv (MkP (px,py,pz)) (MkV (vx,vy,vz)) = (px * vx) + (py * vy) + (pz * vz)
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
newtype AffineMatrix = A (Matrix Double) deriving (Eq)


instance Show AffineMatrix where

  show (A m) = show m

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
stateTransform ::    AffineState       -- ^ The affine state to apply
                  -> TransformMatrices
stateTransform st = applyTransforms affineId $ newAffine st


-- Given an affine matrix m, this function generates a triple of matrix 
-- transformations:
--   (m, inv(m), transpose(inv(m)))
makeTransforms ::    AffineMatrix
                  -> TransformMatrices
makeTransforms m = (m,mi,mit)
  where
    mi  = affineInvert m
    mit = affineTranspose mi


-- | Given a transformation affine matrix t and an initial affine matrix m,
-- this function applies t to m, yielding m' which is used to generate
-- a new triple:
applyTransforms ::    AffineMatrix      -- ^ The transformation affine matrix
                   -> AffineMatrix      -- ^ The initial affine matrix
                   -> TransformMatrices
applyTransforms t m = makeTransforms $ m * t


-- | Symbolic operator alias for applyTransforms
(<!~) ::    AffineMatrix      -- ^ The transformation affine matrix
         -> AffineMatrix      -- ^ The initial affine matrix
         -> TransformMatrices
(<!~) = applyTransforms


-- | (<!~) with flipped operands
(~!>) ::    AffineMatrix      -- ^ The initial affine matrix
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
  (<!-)  ::    a             -- ^ The object to transform
            -> AffineMatrix  -- ^ The transformation matrix
            -> a

  -- | "Flipped" version of the (-!>) operator that takes an AffineState 
  -- instance first, then the object to transform
  (-!>)  ::    AffineMatrix -- ^ The transformation matrix
            -> a            -- ^ The object to transform
            -> a
  (-!>) = flip (<!-)

  -- | Basically, this is the "do" notation equivalent of the (<!-) operator.
  (<!=) ::    a           -- ^ The object to transform
           -> AffineState -- ^ The intermediate transformation state
           -> a
  (<!=) obj st = obj <!- (execState st $ affineId)

  -- | "Flipped" version of the (=!>) operator that takes an AffineState 
  -- instance first, then the object to transform
  (=!>) ::    AffineState -- ^ The intermediate transformation state
           -> a           -- ^ The object to transform
           -> a
  (=!>) = flip (<!=)

  -- | Like the (<!=), but allows for a user-defined affine matrix to be passed
  -- in, rather than using the affine idenity matrix, affineId, as the starting
  -- point
  withTransform ::    a
                   -> AffineMatrix
                   -> AffineState
                   -> a
  withTransform obj m st = obj <!- (execState st $ m)

--------------------------------------------------------------------------------

-- | Creates a new 4x4 affine transformation matrix
affineId :: AffineMatrix
affineId = A $ M.unit 4


-- | Converts an affine matrix to a list of doubles
toList :: AffineMatrix -> [[Double]] 
toList (A m) = M.toList m 


-- | Converts a list of doubles to an affine matrix
fromList ::  [[Double]] -> AffineMatrix
fromList components = A $ M.fromList components 


instance Monoid AffineMatrix where

  mempty  = affineId

  mappend = (*)


instance Num AffineMatrix where
  
  (+) (A m) (A n) = A $ m + n
  
  (-) (A m) (A n) = A $ m - n
  
  (*) (A m) (A n) = A $ m * n
  
  abs (A m) = A $ M.map abs m
  
  signum (A m) = A $ M.map signum m
  
  negate (A m) = A $ M.map negate m
  
  fromInteger _ = error "fromInteger not supported for matrices"


-- | Applies an affine translation operation to the supplied affine matrix
affineTranslate ::    Double       -- ^ The x translation component
                   -> Double       -- ^ The x translation component
                   -> Double       -- ^ The x translation component
                   -> AffineMatrix -- ^ The affine matrix to translate
                   -> AffineMatrix
affineTranslate tx ty tz (A m) = A $ m + (M.fromList [[0,0,0,tx]
                                                     ,[0,0,0,ty]
                                                     ,[0,0,0,tz]
                                                     ,[0,0,0,0]])


-- | Applies an affine rotation operation to the supplied affine matrix
affineRotate ::    Double       -- ^ The x rotation component in degrees
                -> Double       -- ^ The y rotation component in degrees
                -> Double       -- ^ The z rotation component in degrees
                -> AffineMatrix -- ^ The affine matrix to rotation
                -> AffineMatrix
affineRotate rx ry rz (A m) =
  let unit4 = M.unit 4 
      rxM   = if rx /= 0 then xMat $ deg2rad rx else unit4
      ryM   = if ry /= 0 then yMat $ deg2rad ry else unit4
      rzM   = if rz /= 0 then zMat $ deg2rad rz else unit4
  in  A $ m `M.times` rxM `M.times` ryM `M.times` rzM
  where
    xMat x = M.fromList [[1.0,   0.0,   0.0, 0.0]
                        ,[0.0, cos x, sin x, 0.0]
                        ,[0.0,-sin x, cos x, 0.0]
                        ,[0.0,   0.0,   0.0, 1.0]]

    yMat y = M.fromList [[cos y, 0.0, -sin y, 0.0]
                        ,[0.0,   1.0,    0.0, 0.0]
                        ,[sin y, 0.0,  cos y, 0.0]
                        ,[0.0,   0.0,    0.0, 1.0]]

    zMat z = M.fromList [[ cos z, sin z, 0.0, 0.0]
                        ,[-sin z, cos z, 0.0, 0.0]
                        ,[0.0,   0.0,    1.0, 0.0]
                        ,[0.0,   0.0,    0.0, 1.0]]


-- | Applies an affine scaling operation to the supplied affine matrix
affineScale ::   Double              -- ^ x scale component to scale by
               -> Double              -- ^ y scale component to scale by
               -> Double             -- ^ z scale component to scale by
               -> AffineMatrix -- ^ The affine matrix to scale
               -> AffineMatrix
affineScale sx sy sz (A m) = A $ m * (M.diag [sx, sy, sz, 1])


-- | Inverts the given affine matrix
affineInvert ::    AffineMatrix -- ^ The affine matrix to invert
                -> AffineMatrix
affineInvert (A m) = 
  case (inv m) of
    Just m'  -> A m'
    Nothing  -> error "Affine matrix not invertible!"


-- | Transposes the given affine matrix
affineTranspose ::    AffineMatrix -- ^ The affine matrix to transpose
                   -> AffineMatrix
affineTranspose (A m) = A $ M.transpose m


-- | Apply an affine transformation to a vector
instance AffineTransformable VectorR3 where

  (<!-) (MkV (vx,vy,vz)) (A m) = MkV (x',y',z')
    where
      x' = (c vx (1,1)) + (c vy (1,2)) + (c vz (1,3))
      y' = (c vx (2,1)) + (c vy (2,2)) + (c vz (2,3))
      z' = (c vx (3,1)) + (c vy (3,2)) + (c vz (3,3))
      c v loc | v == 0 = 0
              | v == 1 = m `M.at` loc
              | otherwise = v * (m `M.at` loc)


-- | Apply an affine transformation to a point
instance AffineTransformable PointR3 where

  (<!-) (MkP (px,py,pz)) (A m)
    | w' /= 0 && w' /= 1 = MkP (x' / w',y' / w',z' / w')
    | otherwise          = MkP (x',y',z')
    where
      c v loc | v == 0 = 0
              | v == 1 = m `M.at` loc
              | otherwise = v * (m `M.at` loc)
      x' = (c px (1,1)) + (c py (1,2)) + (c pz (1,3)) + (m `M.at` (1,4))
      y' = (c px (2,1)) + (c py (2,2)) + (c pz (2,3)) + (m `M.at` (2,4))
      z' = (c px (3,1)) + (c py (3,2)) + (c pz (3,3)) + (m `M.at` (3,4))
      w' = (c px (4,1)) + (c py (4,2)) + (c pz (4,3)) + (m `M.at` (4,4))

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

--------------------------------------------------------------------------------

runTests :: IO Counts
runTests = 
  runTestTT $ TestList 
  ["vec3getX" ~: (getX $ vec3 1.0 2.0 3.0) ~?= 1.0
  ,"vec3getY" ~: (getY $ vec3 1.0 2.0 3.0) ~?= 2.0
  ,"vec3getZ" ~: (getZ $ vec3 1.0 2.0 3.0) ~?= 3.0
  ,"vec3+" ~: (vec3 2 5 (-4) + vec3 (-2) (-3) (-5)) ~?= vec3 0 2 (-9)
  ,"vec3-" ~: (vec3 2 5 (-4) - vec3 (-2) (-3) (-5)) ~?= vec3 4 8 1
  ,"vec3*" ~: (vec3 2 5 (-4) * vec3 (-2) (-3) (-5)) ~?= vec3 (-4) (-15) 20
  ,"vec3abs" ~: (abs $ vec3 2 5 (-4)) ~?= vec3 2 5 4
  ,"vec3signum" ~: (signum $ vec3 2 0 (-4)) ~?= vec3 1 0 (-1)
  ,"vec3negate" ~: (negate $ vec3 2 5 (-4)) ~?= vec3 (-2) (-5) 4
  ,"vec3/" ~: (vec3 2 5 (-4) / vec3 (-2) (-3) (-5)) ~?= vec3 (-1) (-5/3) (4/5)
  ,"vec3*." ~: (vec3 2 5 (-4) *. 3) ~?= vec3 (6) 15 (-12)
  ,"vec3/." ~: (vec3 2 5 (-4) /. 2) ~?= vec3 1 (5/2) (-2)
  ,"vec3|." ~: (vec3 2 5 (-4)) |.  vec3 (-2) (-3) (-5) ~?= 1
  ,"vec3mag1" ~: (mag $ vec3 2 3 5) ~?= 6.164414002968976
  ,"vec3mag2" ~: (mag $ vec3 2 (-3) (-5)) ~?= 6.164414002968976
  ,"vec3cross1" ~: (vec3 2 1 (-1) `cross` vec3 (-3) 4 1 ~?= vec3 5 1 11)
  ,"vec3cross" ~: (vec3 (-3) 4 1 `cross` vec3 2 1 (-1)) ~?= vec3 (-5) (-1) (-11)]