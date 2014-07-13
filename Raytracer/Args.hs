{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Raytracer.Args (RuntimeOptions(..)
  ,parse3Tuple
  ,optQuiet, optBounding, optLookAt, optLocation, optFov
  ,optOutputFile, optWidth, optHeight, optBlockSize, optReflect, optRefract
  ,optShading, optShadows, optMaxDepth)
where
import Data.Text hiding (map)
import Control.Lens (makeLenses)
import Control.Applicative
import Options

--------------------------------------------------------------------------------

data RuntimeOptions = RuntimeOptions 
  { _optQuiet       :: Bool
  , _optBounding    :: Bool
  , _optOutputFile  :: String
  , _optLocation    :: String
  , _optLookAt      :: String
  , _optFov         :: Double
  , _optWidth       :: Int
  , _optHeight      :: Int
  , _optBlockSize   :: Int
  , _optReflect     :: Int
  , _optRefract     :: Int
  , _optShading     :: Int
  , _optShadows     :: Int
  , _optMaxDepth    :: Int
}

makeLenses ''RuntimeOptions

--------------------------------------------------------------------------------

instance Options RuntimeOptions where

  defineOptions = pure RuntimeOptions

    <*> defineOption optionType_bool (\o -> o
      { optionShortFlags  = ['q']
      , optionLongFlags   = ["quiet"]
      , optionDefault     = False
      , optionDescription = "Suppresses most program output"
      })

    <*> defineOption optionType_bool (\o -> o
      { optionShortFlags  = ['B']
      , optionLongFlags   = ["bounding"]
      , optionDefault     = False
      , optionDescription = "Render AABBs (bounding boxes)"
      })

    <*> simpleOption "outputFile" "/tmp/test.bmp"
        "Image output file to write"

    <*> defineOption optionType_string (\o -> o
      { optionShortFlags  = ['l']
      , optionLongFlags   = ["location"]
      , optionDefault     = "0,0.5,-4.0"
      , optionDescription = "The camera's default world location"
      })

    <*> defineOption optionType_string (\o -> o
      { optionShortFlags  = ['a']
      , optionLongFlags   = ["lookAt"]
      , optionDefault     = "0.0, 0.5, 0.0"
      , optionDescription = "The camera's default 'look at' position"
      })

    <*> defineOption optionType_double (\o -> o
      { optionShortFlags  = ['f']
      , optionLongFlags   = ["fov"]
      , optionDefault     = 60
      , optionDescription = "Sets the camer'a field-of-view (FOV) value"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['w']
      , optionLongFlags   = ["width"]
      , optionDefault     = 120
      , optionDescription = "Defines image width"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['h']
      , optionLongFlags   = ["height"]
      , optionDefault     = 80
      , optionDescription = "Defines image height"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['b']
      , optionLongFlags   = ["blockSize"]
      , optionDefault     = 1
      , optionDescription = "Sets the rendered image interpolation block size"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['r']
      , optionLongFlags   = ["reflect"]
      , optionDefault     = 1
      , optionDescription = "Enables/disables reflection"
      })

    <*> defineOption optionType_int (\o -> o
      { optionLongFlags   = ["refract"]
      , optionDefault     = 1
      , optionDescription = "Enables/disables refraction"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['z']
      , optionLongFlags   = ["shading"]
      , optionDefault     = 1
      , optionDescription = "Enables/disables shading"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['s']
      , optionLongFlags   = ["shadows"]
      , optionDefault     = 1
      , optionDescription = "Enables/disables shadows"
      })

    <*> defineOption optionType_int (\o -> o
      { optionShortFlags  = ['D']
      , optionLongFlags   = ["depth"]
      , optionDefault     = 3
      , optionDescription = "Sets the maximum recursion depth for ray spawning"
      })


-- | Parses a string into a 3-tuple of doubles
parse3Tuple :: String -> (Double,Double,Double)
parse3Tuple s = 
  case (map (\x -> read x :: Double) parts') of
    [a,b,c] -> (a,b,c)
    _       -> error $ "parse3Tuple :: Bad input: " ++ s
  where
    parts' = map (unpack . strip) $ split (== ',') (pack s)

