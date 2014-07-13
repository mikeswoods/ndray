{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Raytracer.Args 
  (
   RuntimeOptions(..)
  ,optQuiet
  ,optBounding
  ,optLookAt
  ,optLocation
  ,optFov
  ,optOutputFile
  ,optWidth
  ,optHeight
  ,optBlockSize
  ,optReflect
  ,optRefract
  ,optShading
  ,optShadows
  ,optMaxDepth
  )
where 
import Raytracer.World.Const
  (
   R3Coord
  )
import Text.Read
  (
   readMaybe
  )
import Control.Lens (makeLenses)
import Options

--------------------------------------------------------------------------------

data RuntimeOptions = RuntimeOptions 
  { _optQuiet       :: Bool
  , _optBounding    :: Bool
  , _optOutputFile  :: String
  , _optLocation    :: R3Coord
  , _optLookAt      :: R3Coord
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

instance Show RuntimeOptions where

  show opts = "RuntimeOptions {\n" ++ 
              "  quiet      = " ++ (show $ _optQuiet opts) ++ "\n" ++
              "  bounding   = " ++ (show $ _optBounding opts) ++ "\n" ++
              "  outputFile = " ++ (show $ _optOutputFile opts) ++ "\n" ++
              "  location   = " ++ (show $ _optLocation opts) ++ "\n" ++
              "  lookAt     = " ++ (show $ _optLookAt opts) ++ "\n" ++
              "  fov        = " ++ (show $ _optFov opts) ++ "\n" ++
              "  width      = " ++ (show $ _optWidth opts) ++ "\n" ++
              "  height     = " ++ (show $ _optHeight opts) ++ "\n" ++
              "  blockSize  = " ++ (show $ _optBlockSize opts) ++ "\n" ++
              "  reflect    = " ++ (show $ _optReflect opts) ++ "\n" ++
              "  refract    = " ++ (show $ _optRefract opts) ++ "\n" ++
              "  shading    = " ++ (show $ _optShading opts) ++ "\n" ++
              "  shadows    = " ++ (show $ _optShadows opts) ++ "\n" ++
              "  maxDepth   = " ++ (show $ _optMaxDepth opts) ++ "\n" ++
              "}" 

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

    <*> defineOption optionType_R3Coord (\o -> o
      { optionShortFlags  = ['l']
      , optionLongFlags   = ["location"]
      , optionDefault     = (0, 0.5, -4.0)
      , optionDescription = "The camera's default world location"
      })

    <*> defineOption optionType_R3Coord (\o -> o
      { optionShortFlags  = ['a']
      , optionLongFlags   = ["lookAt"]
      , optionDefault     = (0.0, 0.5, 0.0)
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


parseTriple :: String -> Either String R3Coord
parseTriple s = 
  case (readMaybe s :: Maybe R3Coord) of
    Just t  -> Right t
    Nothing -> Left $ "Bad input: " ++ s


-- | Parses a string into a 3-tuple of doubles
optionType_R3Coord :: OptionType R3Coord
optionType_R3Coord = optionType "optionType_R3Coord" (0.0, 0.0, 0.0) parseTriple show
