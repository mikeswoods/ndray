{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Raytracer.World.Color 
  (WorldColor
  ,limit
  ,r
  ,g
  ,b
  ,worldColor
  ,worldColor8
  ,toBytes
  ,toInts
  ,(.*.)
  ,(./.)
  ,lightness
  ,average
  ,luminosity
  ,intensity
  ,interpolate
  ,lerp
  ,cerp
  ,black
  ,gray
  ,silver
  ,white
  ,maroon
  ,red
  ,olive
  ,yellow
  ,green
  ,lime
  ,teal
  ,aqua
  ,navy
  ,blue
  ,purple
  ,fuchsia
  ,basicColors)
where
import Data.Typeable 
  (
   Typeable
  )
import System.Random 
  (
   Random(..)
  )
import Data.Word 
  (
   Word8
  )
import Control.Lens 
  (
   Lens'
  ,lens
  ,(^.)
  )
import Text.Printf 
  (
   printf
  )
import qualified Raytracer.Utils.Math as Math

--------------------------------------------------------------------------------

-- | Color datatype representing the final color value that will ultimately be
-- rendered. This is a simple RGB color encoding scheme that is independent
-- than any library color representation, e.g. JuicyPixels PixelRGB8 type

-- Deriving notes adapted from the article 
-- "How do I write a Data.Vector.Unboxed instance in Haskell?" at
-- http://stackoverflow.com/a/10866718
newtype WorldColor = 
  WorldColor (Word8, Word8, Word8)
  deriving (Eq, Ord, Typeable)


-- | Limits any integer value given to the 0-255 byte range
limit :: Int -> Word8
limit x = fromIntegral (Math.clamp 0 255 $ x) :: Word8
{-# INLINE limit #-}


-- | Sets/gets the red component of a color
r :: Lens' WorldColor Word8
r = lens getter setter
  where
    getter (WorldColor (r',_,_))      = r'
    setter (WorldColor (_,g',b')) r'' = WorldColor (r'',g',b')


-- | Sets/gets the green component of a color
g :: Lens' WorldColor Word8
g = lens getter setter
  where
    getter (WorldColor (_,g',_))      = g'
    setter (WorldColor (r',_,b')) g'' = WorldColor (r',g'',b')


-- | Sets/gets the blue component of a color
b :: Lens' WorldColor Word8
b = lens getter setter
  where
    getter (WorldColor (_,_,b'))      = b'
    setter (WorldColor (r',g',_)) b'' = WorldColor (r',g',b'')


-- | Returns a 3-tuple of RGB values encoding the color as Word8 components
toBytes :: WorldColor -> (Word8, Word8, Word8)
toBytes (WorldColor components) = components


-- | Returns a 3-tuple of RGB values encoding the color as Int components
toInts :: WorldColor -> (Int, Int, Int)
toInts c = (rInt,gInt,bInt)
  where
    rInt = (fromIntegral $ c^.r :: Int)
    gInt = (fromIntegral $ c^.g :: Int)
    bInt = (fromIntegral $ c^.b :: Int)


instance Show WorldColor where

  show color = printf "#%0.2x%0.2x%0.2x" (color^.r) (color^.g) (color^.b)


-- | Like worldColor, but with native byte values (Word8)
worldColor8 ::    
     Word8      -- ^ Red (R) color component
  -> Word8      -- ^ Green (G) color component
  -> Word8      -- ^ Blue (B) color component
  -> WorldColor 
worldColor8 rVal gVal bVal = WorldColor (rVal,gVal,bVal)


-- | Constructs a new world color instance with Int values
worldColor ::    
     Int        -- ^ Red (R) color component
  -> Int        -- ^ Green (G) color component
  -> Int        -- ^ Blue (B) color component
  -> WorldColor 
worldColor rVal gVal bVal = WorldColor (limit rVal,limit gVal,limit bVal)


instance Num WorldColor where

  (+) !(WorldColor (r1,g1,b1)) !(WorldColor (r2,g2,b2)) =
    worldColor (Math.w2i r1 + Math.w2i r2) 
               (Math.w2i g1 + Math.w2i g2) 
               (Math.w2i b1 + Math.w2i b2)
  {-# INLINE (+) #-}

  (-) !(WorldColor (r1,g1,b1)) !(WorldColor (r2,g2,b2)) =
    worldColor (Math.w2i r1 - Math.w2i r2) 
               (Math.w2i g1 - Math.w2i g2) 
               (Math.w2i b1 - Math.w2i b2)
  {-# INLINE (-) #-}

  (*) !(WorldColor (r1,g1,b1)) !(WorldColor (r2,g2,b2)) =
    worldColor (Math.w2i r1 * Math.w2i r2) 
               (Math.w2i g1 * Math.w2i g2) 
               (Math.w2i b1 * Math.w2i b2)
  {-# INLINE (*) #-}

  negate _ = error "negate operation not supported for WorldColor"

  abs _ = error "abs operation not supported for WorldColor"

  signum _ = error "signum operation not supported for WorldColor"

  fromInteger _ = error "fromInteger operation not supported for WorldColor" 


instance Monoid WorldColor where

  mempty  = black

  mappend = (+) 


(.*.) :: WorldColor -> Double -> WorldColor
color .*. amt
  | amt == 1  = color
  | amt == 0  = black
  | otherwise = WorldColor (rv,gv,bv)
  where
    rv | r' == 0   = 0
       | otherwise = floor $ r' * amt
    gv | g' == 0   = 0
       | otherwise = floor $ g' * amt
    bv | b' == 0   = 0
       | otherwise = floor $ b' * amt
    r' = (fromIntegral $ color^.r) :: Double 
    g' = (fromIntegral $ color^.g) :: Double
    b' = (fromIntegral $ color^.b) :: Double


(./.) :: WorldColor -> Double -> WorldColor
color ./. amt = color .*. (1.0 / amt)

--------------------------------------------------------------------------------

-- Various methods for converting a color to grayscale:
-- Adapted from 
-- http://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/

lightness :: WorldColor -> WorldColor
lightness color = worldColor c c c
  where
    c  = truncate $ lightness' color


lightness' :: WorldColor -> Double
lightness' color = 
  ((maximum [r',g',b']) + (minimum [r',g',b'])) / 2.0
  where 
    r' = (fromIntegral $ color^.r) :: Double
    g' = (fromIntegral $ color^.g) :: Double
    b' = (fromIntegral $ color^.b) :: Double


luminosity :: WorldColor -> WorldColor
luminosity color = worldColor c c c
  where 
    c  = truncate $ luminosity' color


luminosity' :: WorldColor -> Double
luminosity' color = (0.21 * r') + (0.72 * g') + (0.07 * b')
  where 
    r' = (fromIntegral $ color^.r) :: Double
    g' = (fromIntegral $ color^.g) :: Double
    b' = (fromIntegral $ color^.b) :: Double


-- | Given a color, then function calculates its intensity on a scale from
-- 0.0 to 1.0
intensity :: WorldColor -> Double
intensity = luminosity'


-- | Averages a list of WorldColors into a single WorldColor, taking into
-- account component Math.clamping by converting each component into an int
-- prior to summing them
average ::
     [WorldColor]
  -> WorldColor
average colors = worldColor (floor r'') (floor g'') (floor b'')
  where
    n = fromIntegral $ length colors :: Double
    combine (r1,g1,b1) (r2,g2,b2) = (r1+r2,g1+g2,b1+b2)
    (r',b',g') = foldr combine (0,0,0) $ map toInts colors
    r''        = (fromIntegral r' :: Double) / n
    g''        = (fromIntegral g' :: Double) / n
    b''        = (fromIntegral b' :: Double) / n


-- | Interpolate between 2 colors
interpolate :: 
     ((Double,Double) -> Double -> Double) -- ^ Interpolation function 
  -> WorldColor
  -> WorldColor
  -> Double 
  -> WorldColor
interpolate f c1 c2 mu =
  worldColor (ceiling $ f (r1',r2') mu) 
             (ceiling $ f (g1',g2') mu) 
             (ceiling $ f (b1',b2') mu)
  where 
    (r1,g1,b1) = toInts c1
    (r2,g2,b2) = toInts c2
    r1'        = fromIntegral r1 :: Double
    g1'        = fromIntegral g1 :: Double
    b1'        = fromIntegral b1 :: Double
    r2'        = fromIntegral r2 :: Double
    g2'        = fromIntegral g2 :: Double
    b2'        = fromIntegral b2 :: Double


-- | Linear interpolation between 2 colors
lerp :: 
     WorldColor
  -> WorldColor
  -> Double 
  -> WorldColor
lerp = interpolate Math.lerp


-- | Cosine interpolation between 2 colors
cerp :: 
     WorldColor
  -> WorldColor
  -> Double 
  -> WorldColor
cerp = interpolate Math.cerp

--------------------------------------------------------------------------------

instance Random WorldColor where

  random = randomR (black,white)

  randomR (WorldColor (r1,g1,b1),WorldColor (r2,b2,g2)) rg = 
    (WorldColor (r',g',b'),rg3)
    where
      (r',rg1) = randomR (r1,r2) rg
      (g',rg2) = randomR (g1,g2) rg1
      (b',rg3) = randomR (b1,b2) rg2

--------------------------------------------------------------------------------

-- Some sample colors:

black, gray, silver, white, maroon, red, olive, yellow, green, lime, teal, 
  aqua, navy, blue, purple, fuchsia :: WorldColor


black   = worldColor 0 0 0
gray    = worldColor 128 128 128
silver  = worldColor 192 192 192
white   = worldColor 255 255 255
maroon  = worldColor 128 0 0
red     = worldColor 255 0 0
olive   = worldColor 128 128 0
yellow  = worldColor 255 255 0
green   = worldColor 0 128 0
lime    = worldColor 0 255 0
teal    = worldColor 0 128 128
aqua    = worldColor 0 255 255
navy    = worldColor 0 0 128
blue    = worldColor 0 0 255
purple  = worldColor 128 0 128
fuchsia = worldColor 255 0 255


basicColors :: [WorldColor]
basicColors = [black
              ,gray
              ,silver
              ,white
              ,maroon
              ,red
              ,olive
              ,yellow
              ,green
              ,lime
              ,teal
              ,aqua
              ,navy
              ,blue
              ,purple
              ,fuchsia]
