{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.Surface.Material (Material(..)
  ,defaultMaterial
  ,ambient, diffuse, opacity, reflect, refract, shadow, shine, specular)
where
import Text.Printf (printf)
import Data.Monoid (Monoid(..))
import Control.Monad.State (execState)
import Control.Lens (makeLenses, (^.), (.=), (%=))

--------------------------------------------------------------------------------

data Material = 
  Material                -- See the following articles for explanations:
  { _ambient  :: Double   -- http://www.povray.org/documentation/view/3.6.1/345/
  , _diffuse  :: Double   -- http://www.povray.org/documentation/view/3.6.0/346/
  , _opacity  :: Double
  , _reflect  :: Double   -- http://www.povray.org/documentation/view/3.7.0/348/
  , _refract  :: Double   -- http://www.povray.org/documentation/view/3.6.0/414/
  , _shadow   :: Bool     -- 
  , _shine    :: Double
  , _specular :: Double } -- http://www.povray.org/documentation/view/3.6.1/347/
  deriving (Eq)

-- Lens definitions for the fields of Material
-- See http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html
-- for details on using the Control.Lens library

makeLenses ''Material

instance Show Material where

  show m = 
    printf ("Material {\n" ++ 
            "  ambient  = %.2f\n" ++ 
            "  diffuse  = %.2f\n" ++
            "  opacity  = %.2f\n" ++ 
            "  reflect  = %.2f\n" ++ 
            "  refract  = %.2f\n" ++ 
            "  shadow   = %.2f\n" ++ 
            "  shine    = %.2f\n" ++ 
            "  specular = %.2f\n" ++ 
            "}\n")
           (m^.ambient)
           (m^.diffuse)
           (m^.opacity)
           (m^.reflect)
           (m^.refract)
           (m^.specular)
           (m^.shine)

--------------------------------------------------------------------------------

defaultMaterial :: Material
defaultMaterial = Material 
  { _ambient  = 0.1   -- 0.1 => Minimal ambient shading
  , _diffuse  = 0.6   -- 0.6 => Normal diffusion
  , _opacity  = 1.0   -- 1.0 => No transparency
  , _reflect  = 0.0   -- 0.0 => No reflection
  , _refract  = 1.0   -- 1.0 => No refraction
  , _specular = 0.0   -- 0.0 => No high-light
  , _shadow   = True  -- 1.0 => Shadow, anything else no shadow
  , _shine    = 20.0 }


instance Monoid Material where

  mempty     = defaultMaterial

  mappend m' = execState $ do 
    ambient  %= (+ m'^.ambient)
    diffuse  %= (+ m'^.diffuse)
    opacity  %= (+ m'^.opacity)
    reflect  %= (+ m'^.reflect)
    refract  %= (+ m'^.refract)
    specular %= (+ m'^.specular)
    shadow   .= (m'^.shadow)
    shine    %= (+ m'^.shine)

