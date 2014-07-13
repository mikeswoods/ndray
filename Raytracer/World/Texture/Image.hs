{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Raytracer.World.Texture.Image (imagemap)
where
import Control.Exception
import Codec.Picture
import Codec.Picture.Types
import Text.Printf (printf)
import Raytracer.World.Texture (Texture(..), TextureError(..))
import Raytracer.World.Color (WorldColor, worldColor8, (.*.))
import Raytracer.Utils.File (resolvePath)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data ImageMapTexture = ImageMap FilePath (Int,Int) DynamicImage  -- Image map


instance Show ImageMapTexture where
  show (ImageMap fp (w,h) _ ) = printf "ImageMap{%s, (%d,%d)}" fp w h


instance Texture ImageMapTexture where
  getTexel (ImageMap _ (w,h) img) (u,v) _ = 
    case withInterpPx of
      Left errorMessage -> throw $ BadTexel errorMessage (u,v)
      Right pixelValue  -> pixelValue
    where
      w'  = fromIntegral w :: Double
      h'  = fromIntegral h :: Double
      --noInterpPx   = readPixelValue img (floor $ u * w') (floor $ v * h')
      withInterpPx = bilinearInterp img (u,v) w h w' h'


imagemap ::     FilePath   -- Path to the texure file,
             -> IO ImageMapTexture
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

