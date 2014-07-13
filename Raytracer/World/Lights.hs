{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Raytracer.World.Lights (LightSource(PointLight), pointLight
  ,position, color)
where

import Control.Lens (Lens', lens)
import Raytracer.World.Color (WorldColor)
import Raytracer.World.Environment (Coordinate)
import Raytracer.Space.Tensor (PointR3, point3)

--------------------------------------------------------------------------------

data LightSource =
  PointLight PointR3 WorldColor
  deriving (Eq,Show)


-- | Getter/setter lens for <<LightSource>>.position
position :: Lens' LightSource PointR3
position = lens getter setter
  where
    getter (PointLight pos _)    = pos
    setter (PointLight _ c) pos' = PointLight pos' c


-- | Getter/setter lens for <<LightSource>>.color
color :: Lens' LightSource WorldColor
color = lens getter setter
  where
    getter (PointLight _ c)      = c
    setter (PointLight pos _) c' = PointLight pos c'

--------------------------------------------------------------------------------

-- | Creates a simple, directionless "point" light source
pointLight ::    Coordinate 
              -> WorldColor
              -> LightSource
pointLight (px,py,pz) c = PointLight (point3 px py pz) c
