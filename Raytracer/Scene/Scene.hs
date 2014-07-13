{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Scene.Scene (Scene(..), SceneState
  ,sceneObjects, sceneLights, sceneBackground
  ,emptyScene, scene, light, object, background)
where
import Text.Printf (printf)
import Data.Typeable (Typeable)
import Control.Monad.State (State, execState, get, put)
import Control.Lens ((^.), (.=), (.~), makeLenses)
import qualified Raytracer.World.Color as C
import Raytracer.World.Lights (LightSource)
import qualified Raytracer.Surface.Texture as T

--------------------------------------------------------------------------------

-- | The scene instance that contains a collection of scene objects
-- | as well as a background

data Scene a = Scene 
  { _sceneObjects    :: ![a]
  , _sceneLights     :: ![LightSource]
  , _sceneBackground :: !T.Texture
  } deriving (Typeable)

makeLenses ''Scene


instance Show a => Show (Scene a) where

  show s = 
    printf ("Scene {\n" ++
            "  #objects   = %d,\n" ++
            "  #lights    = %d,\n" ++
            "  background = %s\n"  ++
            "}\n") 
           (length $ s^.sceneObjects)
           (length $ s^.sceneLights)
           (show $ s^.sceneBackground)


type SceneState a = State (Scene a) ()

--------------------------------------------------------------------------------

-- | Creates a new, empty scene with no objects, lights, and a black background
emptyScene :: Scene a
emptyScene = Scene 
  { _sceneObjects    = []
  , _sceneLights     = []
  , _sceneBackground = T.pigment C.black }

--------------------------------------------------------------------------------

-- | Used to begin the creation of a scene monadic context
scene ::    SceneState a -- ^ Intermediate scene state
         -> Scene a
scene st = execState st emptyScene


-- | Create a new scene object in a monadic context
object ::    a            -- ^ The object instance to place in the scene
          -> SceneState a
object o = get >>= \s -> put $ sceneObjects .~ (o:s^.sceneObjects) $ s


-- | Create a new scene light in a monadic context
light ::    LightSource  -- ^ The light instance to place in the scene
         -> SceneState a
light l = get >>= \s -> put $ sceneLights .~ (l:s^.sceneLights) $ s


-- | Create a new scene background in a monadic context
background ::    T.Texture    -- ^ The background text to set for the scene
              -> SceneState a
background = (.=) sceneBackground
