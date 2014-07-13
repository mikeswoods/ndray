{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Raytracer.World.Color (WorldColor, limit, r, g, b
  ,worldColor, worldColor8, asByteTriple, asIntTriple
  ,(.*.), (./.)
  ,lightness, average, luminosity, intensity, interpolate, mix
  ,black, gray, silver, white, maroon, red, olive, yellow
  ,green, lime, teal, aqua, navy, blue, purple, fuchsia
  ,basicColors)
where
import System.Random (Random(..))
import GHC.Word (Word8)
import Data.Monoid (Monoid(..))
import Control.Lens (Lens', lens, (^.))
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)
import Raytracer.Utils.Math (clamp, w2i, cosineInterp)

--------------------------------------------------------------------------------

-- | Color datatype representing the final color value that will ultimately be
-- rendered. This is a simple RGB color encoding scheme that is independent
-- than any library color representation, e.g. JuicyPixels PixelRGB8 type

-- Deriving notes adapted from the article 
-- "How do I write a Data.Vector.Unboxed instance in Haskell?" at
-- http://stackoverflow.com/a/10866718
newtype WorldColor = WorldColor (Word8, Word8, Word8)
        deriving (Eq, Ord, Vector U.Vector, MVector U.MVector, U.Unbox)


-- | Limits any integer value given to the 0-255 byte range
limit :: Int -> Word8
limit x = fromIntegral (clamp 0 255 $ x) :: Word8
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
asByteTriple :: WorldColor -> (Word8, Word8, Word8)
asByteTriple (WorldColor components) = components


-- | Returns a 3-tuple of RGB values encoding the color as Int components
asIntTriple :: WorldColor -> (Int, Int, Int)
asIntTriple c = (rInt,gInt,bInt)
  where
    rInt = (fromIntegral $ c^.r :: Int)
    gInt = (fromIntegral $ c^.g :: Int)
    bInt = (fromIntegral $ c^.b :: Int)


instance Show WorldColor where

  show color = printf "#%0.2x%0.2x%0.2x" (color^.r) (color^.g) (color^.b)


-- | Like worldColor, but with native byte values (Word8)
worldColor8 ::    Word8     -- ^ Red (R) color component
               -> Word8     -- ^ Green (G) color component
               -> Word8     -- ^ Blue (B) color component
              -> WorldColor 
worldColor8 rVal gVal bVal = WorldColor (rVal,gVal,bVal)


-- | Constructs a new world color instance with Int values
worldColor ::    Int      -- ^ Red (R) color component
              -> Int      -- ^ Green (G) color component
              -> Int      -- ^ Blue (B) color component
              -> WorldColor 
worldColor rVal gVal bVal = WorldColor (limit rVal,limit gVal,limit bVal)


instance Num WorldColor where

  (+) !(WorldColor (r1,g1,b1)) !(WorldColor (r2,g2,b2)) =
    worldColor (w2i r1 + w2i r2) (w2i g1 + w2i g2) (w2i b1 + w2i b2)
  {-# INLINE (+) #-}

  (-) !(WorldColor (r1,g1,b1)) !(WorldColor (r2,g2,b2)) =
    worldColor (w2i r1 - w2i r2) (w2i g1 - w2i g2) (w2i b1 - w2i b2)

  (*) !(WorldColor (r1,g1,b1)) !(WorldColor (r2,g2,b2)) =
    worldColor (w2i r1 * w2i r2) (w2i g1 * w2i g2) (w2i b1 * w2i b2)

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


average' :: WorldColor -> Double
average' color = 
  ((fromIntegral (color^.r + color^.g + color^.b)) :: Double) / 3.0


average :: WorldColor -> WorldColor
average color = worldColor c c c
  where 
    c = truncate $ average' color


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


-- | Mixes two colors based on a mixture amount between 0 and 1
mix ::    WorldColor
       -> WorldColor
       -> Double 
       -> WorldColor 
mix (WorldColor (r1,g1,b1)) (WorldColor (r2,b2,g2)) amt =
  worldColor (floor $ (r1' * amt') + (r2' * amtP))
             (floor $ (g1' * amt') + (g2' * amtP))
             (floor $ (b1' * amt') + (b2' * amtP))
  where
    amt' = clamp 0.0 1.0 amt
    amtP = 1.0 - amt'
    r1'  = fromIntegral r1 :: Double
    g1'  = fromIntegral g1 :: Double
    b1'  = fromIntegral b1 :: Double
    r2'  = fromIntegral r2 :: Double
    g2'  = fromIntegral g2 :: Double
    b2'  = fromIntegral b2 :: Double


-- | Interpolate between 2 colors
interpolate ::    WorldColor
               -> WorldColor
               -> Double 
               -> WorldColor
interpolate (WorldColor (r1,g1,b1)) (WorldColor (r2,b2,g2)) mu =
  worldColor (floor rv) (floor gv) (floor bv)
  where 
    rv  = cosineInterp (r1',r2') mu
    gv  = cosineInterp (g1',g2') mu
    bv  = cosineInterp (b1',b2') mu
    r1' = fromIntegral r1 :: Double
    g1' = fromIntegral g1 :: Double
    b1' = fromIntegral b1 :: Double
    r2' = fromIntegral r2 :: Double
    g2' = fromIntegral g2 :: Double
    b2' = fromIntegral b2 :: Double

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
