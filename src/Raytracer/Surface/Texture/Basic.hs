{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Surface.Texture.Basic 
  (
   BasicTexture
  ,SineOpts(..)
  ,defaultTexture
  ,defaultBackdrop
  ,pigment
  ,checkerboard
  ,brick
  ,alt
  ,sine
  ,defaultSineOpts
  ,xgradient
  ,ygradient
)
where
import Text.Printf 
  (
   printf
  )
import Data.Typeable 
  (
   Typeable
  )
import Data.Bits 
  (
   (.&.)
  )
import Raytracer.Utils.Math 
  (
   deg2rad
  )
import Raytracer.Surface.Texture 
  (
   Texture(..)
  ,WithTexture(..)
  )
import Raytracer.World.Color 
  (
   WorldColor
  ,white
  ,black
  ,cerp
  )

--------------------------------------------------------------------------------

data BasicTexture = 
    SolidPigment WorldColor
  | CheckerBoard WorldColor WorldColor Double
  | Alt WorldColor WorldColor Double Int
  | Brick WorldColor WorldColor
  | Sine WorldColor WorldColor SineOpts
  | XGradient WorldColor WorldColor Double
  | YGradient WorldColor WorldColor Double
  deriving (Typeable)

-- | OPtions specific to the "Sine" texture
data SineOpts =
 SineOpts 
 { sineAmp    :: Double
 , sineFreq   :: Double 
 , sineOffset :: Double
 , sineAngle  :: Double
 } deriving (Show,Typeable)

instance Show (BasicTexture) where

  show (SolidPigment c) = 
    printf "SolidPigment{%s}" $ show c

  show (CheckerBoard c1 c2 scale) = 
    printf "CheckerBoard{%s,%s %% %.4f }" (show c1) (show c2) scale

  show (Alt c1 c2 scale mu) = 
    printf "Alt{%s, %s %% %.4f %d}" (show c1) (show c2) scale mu

  show (Sine c1 c2 opts) = 
    printf "Sine{%s %s %% %s}" (show c1) (show c2) (show opts)

  show (Brick c1 c2) = 
    printf "Brick{%s %% %s}" (show c1) (show c2)

  show (XGradient c1 c2 _) = 
    printf "XGradient{%s %% %s}" (show c1) (show c2)

  show (YGradient c1 c2 _) = 
    printf "YGradient{%s %% %s}" (show c1) (show c2)

--------------------------------------------------------------------------------

instance WithTexture BasicTexture where

  texel (SolidPigment c) _ = c

  texel (CheckerBoard c1 c2 scale) (u,v)
    | even x' && even y' = c1
    | odd x' && odd y'   = c1
    | otherwise          = c2 
    where
      x' = floor $ u / scale
      y' = floor $ v / scale

  texel (Alt c1 c2 scale mu) (u,v)
    | ((floor $ u / scale) + (floor $ v / scale)) `mod` mu == 0 = c1
    | otherwise = c2

  texel (Sine c1 c2 opts) (u,v) = cerp c1 c2 t
    where
      freq   = sineFreq opts
      offset = sineOffset opts
      amp    = sineAmp opts
      theta  = deg2rad $ sineAngle opts
      uv     = (u * freq * cos theta) + (v * freq * sin theta)
      t      = amp * (abs $ sin $ (uv * pi) + offset)

  -- From http://bentonian.com/Lectures/AdvGraph1314/3.%20Ray%20tracing%20-%20color%20and%20texture.pdf
  texel (Brick c1 c2) (u,v) = if edge then c1 else c2
    where
      tx'    = u * 10.0 :: Double 
      tx     = (floor tx') :: Int
      ty'    = v * 10.0 :: Double 
      ty     = (floor ty') :: Int
      oddity = (tx .&. 0x01) == (ty .&. 0x01)
      edge   = ((tx' - (fromIntegral tx :: Double) < 0.1) && oddity) || 
               ((ty' - (fromIntegral ty :: Double)) < 0.1)
    
  texel (XGradient c1 c2 _) (u,_) = cerp c1 c2 u

  texel (YGradient c1 c2 _) (_,v) = cerp c1 c2 v

--------------------------------------------------------------------------------

-- | Default texture applied to scene objects when they are initially created
defaultTexture :: Texture
defaultTexture = Texture $ SolidPigment white


-- | Default texture backdrop
defaultBackdrop :: Texture
defaultBackdrop = Texture $ SolidPigment black


-- | Solid pigment texture function
pigment ::    
     WorldColor -- ^ The pigment color
  -> Texture
pigment = Texture . SolidPigment


-- | 2-tone checkerboard pattern
checkerboard ::    
     WorldColor -- ^ The first tile color
  -> WorldColor -- ^ The second tile color
  -> Double     -- ^ The tile scale factor
  -> Texture
checkerboard c1 c2 scale = Texture $ CheckerBoard c1 c2 scale


-- | 2-tone variable alternation scheme
alt ::    
     WorldColor -- ^ The first tile color
  -> WorldColor -- ^ The second tile color
  -> Double     -- ^ The tile scale factor
  -> Int        -- ^ "mod" factor
  -> Texture
alt c1 c2 scale mu = Texture $ Alt c1 c2 scale mu


-- | Default sine options:
defaultSineOpts :: SineOpts
defaultSineOpts = SineOpts 1.0 4.0 0.0 90.0


-- | 2-tone checkerboard pattern
sine ::    
     WorldColor -- ^ The first color
  -> WorldColor -- ^ The second color
  -> SineOpts
  -> Texture
sine c1 c2 opts = Texture $ Sine c1 c2 opts


-- | 2-tone checkerboard pattern
brick ::    
     WorldColor -- ^ The first tile color
  -> WorldColor -- ^ The second tile color
  -> Texture
brick c1 c2 = Texture $ Brick c1 c2


-- | 2-color gradient along X
xgradient ::     
     WorldColor -- ^ The "top" gradient color
  -> WorldColor -- ^ The "bottom" gradient color
  -> Double     -- ^ Mixture amount
  -> Texture
xgradient c1 c2 amount = Texture $ XGradient c1 c2 amount


-- | 2-color gradient along Y
ygradient ::    
    WorldColor -- ^ The "left" gradient color
 -> WorldColor -- ^ The "right" gradient color
 -> Double     -- ^ Mixture amount
 -> Texture
ygradient c1 c2 amount = Texture $ YGradient c1 c2 amount
