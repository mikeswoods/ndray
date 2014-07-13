{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.Objects.Attributes 
  (
   Attributes(..)
  ,WithAttributes(..)
  ,attributes
  ,defaultAttrs
  ,surfaceMapping
  ,surfaceMaterial, affine
  ,attrs
  ,textureMap
  ,material
  ,ambient
  ,diffuse
  ,opacity
  ,reflect
  ,refract
  ,shine
  ,specular
  ,shadow
  )
where
import Data.Typeable 
  (
   Typeable
  )
import Control.Lens 
  (
   (.=)
  ,makeLenses
  )
import Control.Monad.State 
  (
   State
  ,execState
  )
import Raytracer.Space.Tensor 
  (
   TransformMatrices
   ,emptyTransforms
   )
import qualified Raytracer.Surface.Mapping as Mapping
import qualified Raytracer.Surface.Material as Material

--------------------------------------------------------------------------------

-- | This type represents a collection of attributes that can be applied to a
-- scene object
data Attributes = Attributes
  { _surfaceMapping  :: Mapping.Mapping
  , _surfaceMaterial :: Material.Material
  , _affine          :: TransformMatrices }
  deriving (Typeable)

makeLenses ''Attributes


-- | Type representing the intermediate results of a sequence 
-- of affine transformations for use in a monadic context
type AttributeState = State Attributes ()

--------------------------------------------------------------------------------

class WithAttributes a where

  attributesOf :: a -> Attributes

--------------------------------------------------------------------------------

instance Show Attributes where

  show _ = "Attributes {texture = ..., material = ..., affine = ... }\n"

--------------------------------------------------------------------------------

attributes :: 
     Mapping.Mapping 
  -> Material.Material 
  -> TransformMatrices 
  -> Attributes
attributes = Attributes


defaultAttrs :: Attributes
defaultAttrs = 
  Attributes 
  { _surfaceMapping  = Mapping.defaultMapping
  , _surfaceMaterial = Material.defaultMaterial
  , _affine          = emptyTransforms }

--------------------------------------------------------------------------------

-- | Creates a new attribute setting context for an object
attrs :: 
     AttributeState 
  -> Attributes
attrs st = execState st $ defaultAttrs


-- | Sets an object's texture in a monadic AttributeState context
textureMap ::     
     Mapping.Mapping 
  -> AttributeState
textureMap t' = surfaceMapping .= t'

-- | Sets an object's material in a monadic AttributeState context
material ::     
     Material.Material 
  -> AttributeState
material m' = surfaceMaterial .=  m'


-- These functions set various individual texture/material properties on an
-- Attribute instance

-- [ambient] :: Controls ambient shading level
ambient ::
     Double
  -> AttributeState
ambient value = surfaceMaterial.Material.ambient .= value


-- [diffuse] :: Controls diffuse shading level
diffuse ::     
     Double
  -> AttributeState
diffuse value = surfaceMaterial.Material.diffuse .= value


-- [opacity] :: Controls material opacity/transparency level
opacity ::     
     Double
  -> AttributeState
opacity value = surfaceMaterial.Material.opacity .= value


-- [reflect] :: Controls the material reflection index
reflect ::
     Double
  -> AttributeState
reflect value = surfaceMaterial.Material.reflect .= value


-- [refract] :: Controls material refraction index
refract ::
     Double
  -> AttributeState
refract value = surfaceMaterial.Material.refract .= value


-- [specular] :: Controls material specular highlights
specular ::
     Double
  -> AttributeState
specular value = surfaceMaterial.Material.specular .= value


-- [shine] :: Controls material shine highlight size
shine ::
     Double
  -> AttributeState
shine value = surfaceMaterial.Material.shine .= value


-- [shadow] :: Determines if the object casts a shadow
shadow ::
     Bool
  -> AttributeState
shadow value = surfaceMaterial.Material.shadow .= value
