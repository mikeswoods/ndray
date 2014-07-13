{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Raytracer.World.Texture.Base (defaultTexture,
  SolidPigmentTexture, pigment, 
  CheckerBoardTexture, checkerboard, 
  BrickTexture, brick, 
  XGradientTexture, xgradient, 
  YGradientTexture, ygradient)
where
import Data.Bits ((.&.))
import Text.Printf (printf)
import Raytracer.World.Texture (Texture(..), TextureError(..))
import Raytracer.World.Color (WorldColor, worldColor8, (.*.)
  ,white, interpolate)

--------------------------------------------------------------------------------

data SolidPigmentTexture = SolidPigment WorldColor


instance Show SolidPigmentTexture where
  show (SolidPigment c) = printf "SolidPigment{%s}" $ show c


instance Texture SolidPigmentTexture where
  getTexel (SolidPigment c) _ _ = c


pigment :: 
     WorldColor -- ^ The pigment color
  -> SolidPigmentTexture
pigment = SolidPigment


defaultTexture :: SolidPigmentTexture
defaultTexture = pigment white

--------------------------------------------------------------------------------

data CheckerBoardTexture = CheckerBoard WorldColor WorldColor Double


instance Show CheckerBoardTexture where
  show (CheckerBoard c1 c2 scale) = 
    printf "CheckerBoard{%s, %s %% %f}" (show c1) (show c2) scale


instance Texture CheckerBoardTexture where
  getTexel (CheckerBoard c1 c2 scale) (u,v) _
    | ((floor $ u / scale) + (floor $ v / scale)) `mod` 2 == 0 = c1
    | otherwise = c2


checkerboard ::    
     WorldColor -- ^ The first tile color
  -> WorldColor -- ^ The second tile color
  -> Double     -- ^ The tile scale factor
  -> CheckerBoardTexture
checkerboard = CheckerBoard

--------------------------------------------------------------------------------

data BrickTexture = Brick WorldColor WorldColor


instance Show BrickTexture where
  show (Brick c1 c2) = printf "Brick{%s, %s}" (show c1) (show c2)


instance Texture BrickTexture where
  getTexel (Brick c1 c2) (u,v) _ = if edge then c1 else c2
    where
      tx'    = u * 10.0 :: Double 
      tx     = (floor tx') :: Int
      ty'    = v * 10.0 :: Double 
      ty     = (floor ty') :: Int
      oddity = (tx .&. 0x01) == (ty .&. 0x01)
      edge   = ((tx' - (fromIntegral tx :: Double) < 0.1) && oddity) || 
               ((ty' - (fromIntegral ty :: Double)) < 0.1)


brick ::    
     WorldColor -- ^ The first tile color
  -> WorldColor -- ^ The second tile color
  -> BrickTexture
brick = Brick

--------------------------------------------------------------------------------

data XGradientTexture = XGradient WorldColor WorldColor Double


instance Show XGradientTexture where
  show (XGradient c1 c2 _) = printf "XGradient{%s, %s}" (show c1) (show c2)


instance Texture XGradientTexture where
  getTexel (XGradient c1 c2 _) (u,_) _ = interpolate c1 c2 u


xgradient ::     
     WorldColor -- ^ The "top" gradient color
  -> WorldColor -- ^ The "bottom" gradient color
  -> Double     -- ^ Mixture amount
  -> XGradientTexture
xgradient = XGradient

--------------------------------------------------------------------------------

data YGradientTexture = YGradient WorldColor WorldColor Double


instance Show YGradientTexture where
  show (YGradient c1 c2 _) = printf "YGradient{%s, %s}" (show c1) (show c2)


instance Texture YGradientTexture where
  getTexel (YGradient c1 c2 _) (_,v) _ = interpolate c1 c2 v


ygradient ::    
     WorldColor -- ^ The "left" gradient color
  -> WorldColor -- ^ The "right" gradient color
  -> Double     -- ^ Mixture amount
  -> YGradientTexture
ygradient = YGradient

--------------------------------------------------------------------------------

