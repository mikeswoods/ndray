{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.Objects.Attributes (Attributes(..), WithAttributes(..)
  ,attributes, defaultAttrs
  ,surfaceTexture, textureMappingShape, textureMappingEntity, surfaceMaterial
  ,affine, attrs, texture, material, ambient, diffuse, opacity
  ,reflect, refract, shine, specular, shadow)
where
import Control.Lens ((.=), makeLenses)
import Control.Monad.State (State, execState)
import Raytracer.Space.Tensor (TransformMatrices, emptyTransforms)
import Raytracer.Surface.Texture
import Raytracer.Surface.Mapping (MappingShape, MappingEntity
  ,defaultMappingShape, defaultMappingEntity)
import qualified Raytracer.Surface.Material as M

--------------------------------------------------------------------------------

-- | This type represents a collection of attributes that can be applied to a
-- scene object
data Attributes = 
  Attributes
  { _surfaceTexture       :: Texture
  , _textureMappingShape  :: MappingShape
  , _textureMappingEntity :: MappingEntity
  , _surfaceMaterial      :: M.Material
  , _affine               :: TransformMatrices }

makeLenses ''Attributes


-- | Type representing the intermediate results of a sequence 
-- of affine transformations for use in a monadic context
type AttributeState = State Attributes ()

--------------------------------------------------------------------------------

class WithAttributes a where

  getAttributes :: a -> Attributes

--------------------------------------------------------------------------------

instance Show Attributes where

  show _ = "Attributes {texture = ..., material = ..., affine = ... }\n"

--------------------------------------------------------------------------------

attributes :: 
     Texture 
  -> MappingShape
  -> MappingEntity
  -> M.Material
  -> TransformMatrices 
  -> Attributes
attributes = Attributes

defaultAttrs :: Attributes
defaultAttrs = 
  Attributes 
  { _surfaceTexture       = defaultTexture
  , _textureMappingShape  = defaultMappingShape
  , _textureMappingEntity = defaultMappingEntity
  , _surfaceMaterial      = M.defaultMaterial
  , _affine               = emptyTransforms }

--------------------------------------------------------------------------------

-- | Creates a new attribute setting context for an object
attrs ::    AttributeState 
         -> Attributes
attrs st = execState st $ defaultAttrs


-- | Sets an object's texture in a monadic AttributeState context
texture ::     Texture 
            -> AttributeState
texture t' = surfaceTexture .= t'

-- | Sets an object's material in a monadic AttributeState context
material ::     M.Material 
             -> AttributeState
material m' = surfaceMaterial .=  m'

-- These functions set various individual texture/material properties on an
-- Attribute instance

-- [ambient] :: Controls ambient shading level
ambient ::     Double
            -> AttributeState
ambient value = surfaceMaterial.M.ambient .= value

-- [diffuse] :: Controls diffuse shading level
diffuse ::     Double
            -> AttributeState
diffuse value = surfaceMaterial.M.diffuse .= value

-- [opacity] :: Controls material opacity/transparency level
opacity ::     Double
            -> AttributeState
opacity value = surfaceMaterial.M.opacity .= value

-- [reflect] :: Controls the material reflection index
reflect ::     Double
            -> AttributeState
reflect value = surfaceMaterial.M.reflect .= value

-- [refract] :: Controls material refraction index
refract ::     Double
            -> AttributeState
refract value = surfaceMaterial.M.refract .= value

-- [specular] :: Controls material specular highlights
specular ::    Double
            -> AttributeState
specular value = surfaceMaterial.M.specular .= value

-- [shine] :: Controls material shine highlight size
shine ::    Double
         -> AttributeState
shine value = surfaceMaterial.M.shine .= value


-- [shadow] :: Determines if the object casts a shadow
shadow ::   Bool
         -> AttributeState
shadow value = surfaceMaterial.M.shadow .= value