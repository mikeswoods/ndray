{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Scene.Scene 
  (
   Scene(..)
  ,SceneState
  ,sceneObjects
  ,sceneLights
  ,sceneEnvironment
  ,emptyScene
  ,scene
  ,light
  ,object
  ,environment
  )
where
import Data.Typeable 
  (
   Typeable
  )
import Control.Monad.State 
  (
   State
  ,execState
  ,get
  ,put
  )
import Control.Lens 
  (
   (^.)
  ,(.=)
  ,(.~)
  ,makeLenses
  )
import Raytracer.Objects.Object 
  (
   WorldObject
  )
import Raytracer.World.Lights 
  (
   LightSource
  )
import Raytracer.Surface.Texture 
  (
   Texture(..)
  )
import Raytracer.Surface.Texture.Basic 
  (
   defaultBackdrop
  )

--------------------------------------------------------------------------------

-- | The scene instance that contains a collection of scene objects
-- | as well as a environment

data Scene = Scene 
  { _sceneObjects     :: ![WorldObject]
  , _sceneLights      :: ![LightSource]
  , _sceneEnvironment :: !Texture
  } deriving (Typeable)

makeLenses ''Scene


type SceneState = State Scene ()

--------------------------------------------------------------------------------

-- | Creates a new, empty scene with no objects, lights, and a black environment
emptyScene :: Scene
emptyScene = Scene 
  { _sceneObjects     = []
  , _sceneLights      = []
  , _sceneEnvironment = defaultBackdrop }

--------------------------------------------------------------------------------

-- | Used to begin the creation of a scene monadic context
scene :: State Scene () -> Scene
scene st = execState st emptyScene


-- | Create a new scene object in a monadic context
object ::    
     WorldObject -- ^ The object instance to place in the scene
  -> SceneState
object o = get >>= \s -> put $ sceneObjects .~ (o:s^.sceneObjects) $ s


-- | Create a new scene light in a monadic context
light ::    
     LightSource  -- ^ The light instance to place in the scene
  -> SceneState
light l = get >>= \s -> put $ sceneLights .~ (l:s^.sceneLights) $ s


-- | Create a scene environment in a monadic context
environment ::    
     Texture    -- ^ The environment text to set for the scene
  -> SceneState
environment = (.=) sceneEnvironment
