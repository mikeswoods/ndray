{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer.Surface.Texture 
  (
   Texture(..)
  ,UVCoord(..), position, face
  ,WithTexture(..)
  ,TextureError(..)
  )
where
import Control.Lens 
  (
   makeLenses
  )
import Data.Typeable 
  (
   Typeable
  )
import Control.Exception
import Raytracer.World.Const 
  (
   R2Coord
  )
import Raytracer.World.Color 
  (
   WorldColor
  )

--------------------------------------------------------------------------------

-- A data type containing the (u,v) coordinate in texture space to display.
-- Face corresponds to the index of the texture to display, as a mapping may
-- have mutiple textures in the case of a cube, for instance.
data UVCoord = 
  UVCoord
  { _position :: R2Coord -- ^ (u,v) texture space position
  , _face     :: Int     -- ^ Texture face index
  } deriving (Show,Eq,Typeable)

makeLenses ''UVCoord

--------------------------------------------------------------------------------

class WithTexture a where

  -- | Computes the texel value at the (u,v) position in texture space
  texel :: 
       a          -- ^ The texture instance to extract a texel value from
    -> R2Coord    -- ^ The (u,v) texture space coordinate to find the value of
    -> WorldColor 

--------------------------------------------------------------------------------

-- | Existentially quantified "box" type used to provide an interface
-- to data types that implement the WithTexture typeclass
data Texture = forall t. (WithTexture t, Typeable t, Show t) => Texture t 
    deriving (Typeable)


instance WithTexture Texture where

  texel (Texture t) = texel t


instance Show Texture where

    show (Texture t) = show t

--------------------------------------------------------------------------------

-- | Generalized texture-related errors
data TextureError =
    BadTexel String R2Coord
  | BadImageMap String
  | MissingTextureFile String
  | FaceIndexOutOfBounds Int
  deriving (Show, Typeable) 

instance Exception TextureError
