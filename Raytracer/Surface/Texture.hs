{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Surface.Texture (Texel(..), Texture, TextureError(..)
  ,defaultTexture
  ,pigment, checkerboard, xgradient, ygradient, brick, imagemap
  ,getTexel)
where
import Control.Exception
import Data.Typeable (Typeable)
import Data.Bits ((.&.))
import Text.Printf (printf)
import Codec.Picture
import Codec.Picture.Types
import Raytracer.World.Color (WorldColor, worldColor8, (.*.)
  ,white, interpolate)
import Raytracer.Utils.File (resolvePath)

--------------------------------------------------------------------------------

data Texture = 
    SolidPigment WorldColor                   -- Single color pigment
  | CheckerBoard WorldColor WorldColor Double -- 2-tone checkerboard pattern
  | Brick WorldColor WorldColor
  | XGradient WorldColor WorldColor Double    -- 2-color gradient along X
  | YGradient WorldColor WorldColor Double    -- 2-color gradient along Y
  | ImageMap FilePath (Int,Int) DynamicImage  -- Image map

data Texture2 = 
    SolidPigment2 WorldColor  


class Texel a where

  texel :: a -> (Double,Double) -> WorldColor


instance Show (Texture) where
    show (SolidPigment c) = 
      printf "SolidPigment{%s}" $ show c
    show (CheckerBoard c1 c2 scale) = 
      printf "CheckerBoard{%s, %s %% %f }" (show c1) (show c2) scale
    show (Brick c1 c2) = 
      printf "Brick{%s, %s}" (show c1) (show c2)
    show (XGradient c1 c2 _) = 
      printf "XGradient{%s, %s}" (show c1) (show c2)
    show (YGradient c1 c2 _) = 
      printf "YGradient{%s, %s}" (show c1) (show c2)
    show (ImageMap fp (w,h) _ ) =
      printf "ImageMap{%s, (%d,%d)}" fp w h


-- | Generalized texture-related errors
data TextureError =
    BadTexel String (Double,Double)
  | BadImageMap String
  | MissingTextureFile String
  deriving (Show, Typeable) 

instance Exception TextureError

--------------------------------------------------------------------------------

-- | Default texture applied to scene objects when they are initially created
defaultTexture :: Texture
defaultTexture = SolidPigment white


-- | Solid pigment texture function
pigment ::    WorldColor -- ^ The pigment color
           -> Texture
pigment = SolidPigment


-- | 2-tone checkerboard pattern
checkerboard ::    WorldColor -- ^ The first tile color
                -> WorldColor -- ^ The second tile color
                -> Double     -- ^ The tile scale factor
                -> Texture
checkerboard = CheckerBoard


-- | 2-tone checkerboard pattern
brick ::    WorldColor -- ^ The first tile color
         -> WorldColor -- ^ The second tile color
         -> Texture
brick = Brick


-- | 2-color gradient along X
xgradient ::     WorldColor -- ^ The "top" gradient color
              -> WorldColor -- ^ The "bottom" gradient color
              -> Double     -- ^ Mixture amount
              -> Texture
xgradient = XGradient


-- | 2-color gradient along Y
ygradient ::    WorldColor -- ^ The "left" gradient color
             -> WorldColor -- ^ The "right" gradient color
             -> Double     -- ^ Mixture amount
             -> Texture
ygradient = YGradient


-- Image map
imagemap ::     FilePath   -- Path to the texure file,
             -> IO Texture
imagemap filePath = 
  -- Either try filePath as an absolute path, and if that fails, try it
  -- relative to the "<Project>/Textures" directory
  do t <- resolvePath Nothing $ Just tryRelPath
     case t of 
       Nothing -> throw $ MissingTextureFile tryRelPath
       Just path -> do
         img <- readImage path
         case img of 
           Left msg   -> throw $ BadImageMap msg 
           Right img' -> let w = dynamicMap imageWidth img'
                             h = dynamicMap imageHeight img'
                          in  return $ ImageMap path (w,h) img'
  where
    tryRelPath = printf "Textures/%s" filePath

--------------------------------------------------------------------------------
















-- | For a given (u,v) coordinate, this function returns a corresponding
-- WorldColor value
getTexel ::    Texture         -- ^ The texture instance
            -> (Double,Double) -- ^ The (u,v) coordinate to map to a texel
            -> WorldColor      -- ^ The worldcolor
getTexel (SolidPigment c) _ = c

getTexel (CheckerBoard c1 c2 scale) (u,v)
  | ((floor $ u / scale) + (floor $ v / scale)) `mod` 2 == 0 = c1
  | otherwise = c2

-- From http://bentonian.com/Lectures/AdvGraph1314/3.%20Ray%20tracing%20-%20color%20and%20texture.pdf
getTexel (Brick c1 c2) (u,v) = if edge then c1 else c2
  where
    tx'    = u * 10.0 :: Double 
    tx     = (floor tx') :: Int
    ty'    = v * 10.0 :: Double 
    ty     = (floor ty') :: Int
    oddity = (tx .&. 0x01) == (ty .&. 0x01)
    edge   = ((tx' - (fromIntegral tx :: Double) < 0.1) && oddity) || 
             ((ty' - (fromIntegral ty :: Double)) < 0.1)
  

getTexel (XGradient c1 c2 _) (u,_) = interpolate c1 c2 u

getTexel (YGradient c1 c2 _) (_,v) = interpolate c1 c2 v
  --interpolate (c1,c2) v

getTexel (ImageMap _ (w,h) img) (u,v) = 
  case withInterpPx of
    Left errorMessage -> throw $ BadTexel errorMessage (u,v)
    Right pixelValue  -> pixelValue
  where
    w'  = fromIntegral w :: Double
    h'  = fromIntegral h :: Double
    --noInterpPx   = readPixelValue img (floor $ u * w') (floor $ v * h')
    withInterpPx = bilinearInterp img (u,v) w h w' h'

-- | Reads the pixel value at the given coordinate
readPixelValue ::    DynamicImage -- ^ The image to read from
                  -> Int          -- ^ The i-th coordinate
                  -> Int          -- ^ The j-th coordinate to read
                  -> Either String WorldColor
readPixelValue (ImageY8 im) i j =
  case (pixelAt im i j) of 
    v' -> Right $ worldColor8 v' v' v'
readPixelValue (ImageY16 _) i j   = 
  Left $ printf "(%d,%d) Not supported: ImageY16" i j
readPixelValue (ImageYF _) i j    = 
  Left $ printf "(%d,%d) Not supported: ImageYF" i j
readPixelValue (ImageYA8 _) i j   = 
  Left $ printf "(%d,%d) supported: ImageYA8" i j
readPixelValue (ImageYA16 _) i j  = 
  Left $ printf "(%d,%d) supported: ImageA16" i j
readPixelValue (ImageRGBF _) i j  = 
  Left $ printf "(%d,%d) supported: ImageRGBF" i j
readPixelValue (ImageRGB8 im) i j = 
  case (pixelAt im i j) of 
    PixelRGB8 r' g' b' -> Right $ worldColor8 r' g' b'
readPixelValue (ImageRGBA8 im) i j =
  case (pixelAt im i j) of 
    PixelRGBA8 r' g' b' _ -> Right $ worldColor8 r' g' b'
readPixelValue (ImageRGB16 _) i j  = 
  Left $ printf "(%d,%d) supported: ImageRGB16" i j
readPixelValue (ImageRGBA16 _) i j = 
  Left $ printf "(%d,%d) supported: ImageRGBA16" i j
readPixelValue (ImageYCbCr8 _) i j = 
  Left $ printf "(%d,%d) supported: ImageYCbCr8" i j
readPixelValue (ImageCMYK8 _) i j  = 
  Left $ printf "(%d,%d) supported: ImageCMYK8" i j
readPixelValue (ImageCMYK16 _) i j = 
  Left $ printf "(%d,%d) supported: ImageCMYK16" i j


instance Texel Texture where
  
  texel = getTexel


instance Texel Texture2 where

  texel (SolidPigment2 c) _ = c


-- Performs bilinear interpolation on the pixel given by a pair of 
-- (U,V) coordinates
-- Adapted from http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_6_Textures_Cameras_and_Speed.shtml
bilinearInterp :: 
     DynamicImage    -- ^ The image
  -> (Double,Double) -- ^ (u,v)
  -> Int             -- ^ width
  -> Int             -- ^ height
  -> Double          -- ^ width prime
  -> Double          -- ^ height prime
  -> Either String WorldColor
bilinearInterp img (u,v) w h w' h' =
  -- So ugly...
  case tryC1 of 
    Right c1 -> 
      case tryC2 of
        Right c2 -> 
          case tryC3 of
            Right c3 ->
              case tryC4 of
                Right c4 -> 
                  Right $ (c1 .*. w1) + (c2 .*. w2) + (c3 .*. w3) + (c4 .*. w4)
                bad4 -> bad4
            bad3 -> bad3
        bad2 -> bad2  
    bad1 -> bad1
  where 
    fu    = (u + 1000.5) * w'
    fv    = (v + 1000.0) * h'
    u1    = (floor fu) `mod` w
    v1    = (floor fv) `mod` h
    u2    = (u1 + 1)   `mod` w
    v2    = (v1 + 1)   `mod` h
    fracu = fu - (fromIntegral $ floor fu :: Double)
    fracv = fv - (fromIntegral $ floor fv :: Double)
    w1    = (1 - fracu) * (1 - fracv)
    w2    = fracu * (1 - fracv)
    w3    = (1 - fracu) * fracv
    w4    = fracu *  fracv
    tryC1 = readPixelValue img u1 v1
    tryC2 = readPixelValue img u2 v1
    tryC3 = readPixelValue img u1 v2
    tryC4 = readPixelValue img u2 v2

