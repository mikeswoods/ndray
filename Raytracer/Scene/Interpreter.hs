{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Raytracer.Scene.Interpreter (loadScene)
where
import Data.IORef
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Scripting.LuaUtils ()
import qualified Scripting.Lua as Lua
import Foreign.C.Types (CInt(..))
import Foreign.Storable (sizeOf, peek, poke)
import Foreign.Ptr (FunPtr, castPtr)
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Control.Exception
import Control.Monad
import Control.Lens ((^.))
import Raytracer.Prelude
import qualified Raytracer.Args as Args

--------------------------------------------------------------------------------

-- | Shorthand for the "standard" parameterized scene type
type StdScene = Scene (WorldObject Primitive)

-- | Mutable object counter type
type Counter = IORef Int

--------------------------------------------------------------------------------

-- | Internal utility functions
liftIO1 :: 
  (a -> omega) -> a -> IO omega 
liftIO1 f arg0 = liftM f $ return arg0

liftIO2 :: 
  (a -> b -> omega) -> a -> b -> IO omega
liftIO2 f arg0 arg1 = liftM2 f (return arg0) (return arg1)

liftIO3 :: 
  (a -> b -> c -> omega) -> a -> b -> c -> IO omega
liftIO3 f arg0 arg1 arg2 = liftM3 f (return arg0) (return arg1) (return arg2)


-- | Compute the normalised index of a value
-- | Borrowed from luautils-0.1.3
normalizeIndex :: 
  Lua.LuaState -> Int -> IO Int
normalizeIndex l i
  | i < 0 = do
      top <- Lua.gettop l
      return $ top + i + 1
  | otherwise = return i


-- | Allocates a new newuserdata value, writes the pointer value of obj
-- to it, and pushes the value onto the stack
pushRef :: 
     Lua.LuaState -- ^ The state to modify
  -> a            -- ^ The value/object to point to
  -> IO ()
pushRef l obj = do
  objPtr <- newStablePtr obj
  p      <- Lua.newuserdata l $ sizeOf objPtr
  poke (castPtr p) objPtr


peekRef :: 
     Lua.LuaState 
  -> Int 
  -> IO (Maybe a)
peekRef l i = do
  ix     <- normalizeIndex l i
  maybeP <- Lua.peek l ix
  case maybeP of
    Just p -> do 
      objPtr <- peek p
      obj    <- deRefStablePtr objPtr
      return $ Just obj
    Nothing -> return Nothing


-- | Pushes the given value onto the stack and sets the named field with the
-- pushed value
setField :: 
  (Lua.StackValue a) 
  => Lua.LuaState -- ^ The state to modify
  -> String       -- ^ The name of the field to set
  -> a            -- ^ The field value
  -> IO ()
setField l name value = do
  Lua.push l $ value
  Lua.setfield l (-2) name


-- | Like setField, except pushRef is used instead of push to push a 
-- pointer to the given value onto the stack
setFieldRef :: 
     Lua.LuaState -- ^ The state to modify
  -> String       -- ^ The name of the field to set
  -> a            -- ^ The field value
  -> IO ()
setFieldRef l name value = do
  pushRef l value
  Lua.setfield l (-2) name


-- | Fetches the given table field from the specified stack index
getField :: 
  (Lua.StackValue a) 
  => Lua.LuaState -- ^ The state to modify
  -> Int          -- ^ The index of the stack value to fetch the given field of
  -> String       -- ^ The name of the field to fetch
  -> a            -- ^ The default value returned if the specified field does not exist
  -> IO a
getField l i name defaultValue = do
  ix <- normalizeIndex l i
  Lua.getfield l ix name        -- Field value pushed onto top of stack
  maybeValue <- Lua.peek l (-1) -- Remove it and move onto the next attribute
  Lua.pop l 1
  case maybeValue of
    Just value -> return value
    Nothing    -> return defaultValue


-- | Gets the specified table field, dereferences it, and returns it
getFieldRef ::
     Lua.LuaState -- ^ The state to modify
  -> Int          -- ^ The index of the stack value to fetch the given field of
  -> String       -- ^ The name of the field to fetch
  -> a            -- ^ The default value returned if the specified field does not exist
  -> IO a
getFieldRef l i name defaultValue = do
  ix <- normalizeIndex l i
  Lua.getfield l ix name       -- Field value pushed onto top of stack
  maybeRef <- Lua.peek l (-1) -- Remove it and move onto the next attribute
  Lua.pop l 1
  case maybeRef of
    Just ref -> do
      stableRef <- peek ref
      obj       <- deRefStablePtr stableRef
      return obj
    Nothing -> return defaultValue


-- | Adds a metatable method to the specified metatable context
setMetaValue :: 
  (Lua.StackValue a)
  => Lua.LuaState -- ^ The Lua state to modify
  -> String       -- ^ The name of the global metatable to add the method to
  -> String       -- ^ The name to associate with the value
  -> a            -- ^ The value itself
  -> IO ()
setMetaValue l metaContextName name value = do
  Lua.newmetatable l metaContextName
  Lua.push l value
  Lua.setfield l (-2) name
  Lua.setmetatable l (-2)


-- | Sets the StackValue's "class" metatable attribute
setClass ::
     Lua.LuaState -- ^ The Lua state to modify
  -> String       -- ^ he name of the global metatable to add the class attribute to
  -> String       -- ^ The class name to assign
  -> IO ()
setClass l metaContextName className = 
  setMetaValue l metaContextName "class" className


-- Read's the StackValue instance's "class" metatable attribute
readClass ::
     Lua.LuaState
  -> Int
  -> IO (Maybe String)
readClass l i = do
  ix <- normalizeIndex l i
  Lua.getmetatable l ix
  Lua.getfield l (-1) "class"
  maybeClassName <- Lua.peek l (-1) :: IO (Maybe String)
  case maybeClassName of
    Just className -> do
      Lua.pop l 1  -- Remove the className
      Lua.pop l ix -- Remove the metatable
      return $ Just className
    Nothing -> do
      Lua.pop l ix -- Remove the metatable
      return Nothing


-- | Adds a metatable method to the specified metatable context
setMetaMethod :: 
     Lua.LuaState            -- ^ The Lua state to modify
  -> String                  -- ^ The name of the global metatable to add the method to
  -> String                  -- ^ The name to associate with the function
  -> FunPtr Lua.LuaCFunction -- ^ The Haskell function itself
  -> IO ()
setMetaMethod l metaContextName methodName f = do
  Lua.newmetatable l metaContextName
  Lua.push l f
  Lua.setfield l (-2) methodName
  Lua.setmetatable l (-2)


-- Pops a triple of Doubles off of the given stack
popCoordTriple :: Lua.LuaState -> IO (Maybe (Double,Double,Double))
popCoordTriple l = do
  maybeZ <- Lua.peek l (-1) :: IO (Maybe Double)
  maybeY <- Lua.peek l (-2) :: IO (Maybe Double)
  maybeX <- Lua.peek l (-3) :: IO (Maybe Double)
  let xyz = catMaybes [maybeX,maybeY,maybeZ]
  if (length xyz) == 3 
    then do 
      let [x',y',z'] = xyz
      Lua.pop l 1
      Lua.pop l 1
      Lua.pop l 1
      return $ Just (x',y',z')
    else return Nothing


-- | Converts a list of 3 items into a 3-tuple (triple). In all other cases
-- an exception is thrown
unsafeListToTriple :: [a] -> (a,a,a)
unsafeListToTriple xs =
  case xs of 
    [x,y,z] -> (x,y,z)
    _       -> throw $ InvalidArgument $ "Bad list length: " ++ (show $ length xs)


-- | 
incObjectCount :: Counter -> IO Int
incObjectCount ref = do
  i <- readIORef ref
  modifyIORef' ref (+1)
  return i

--------------------------------------------------------------------------------

data InterpreterError =
    InvalidArgument String
  | MissingArgument String
  | MissingIndex String
  | UnsupportedType String
  | UndefinedValue String
  | RuntimeError Int String
  deriving (Show, Typeable)

instance Exception InterpreterError

--------------------------------------------------------------------------------
-- [Export] PointR3
--------------------------------------------------------------------------------

showPointR3 :: IO (FunPtr Lua.LuaCFunction)
showPointR3 = Lua.newcfunction $ liftIO1 (show :: PointR3 -> String)


instance Lua.StackValue PointR3 where

  push l p = do
    pushRef l p
    __tostring <- showPointR3
    setMetaMethod l "PointR3" "__tostring" __tostring
    setClass l "PointR3" "PointR3"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA


export_Point :: 
  Double -> Double -> Double -> IO PointR3
export_Point = liftIO3 point3

--------------------------------------------------------------------------------
-- [Export] VectorR3
--------------------------------------------------------------------------------

showVectorR3 :: IO (FunPtr Lua.LuaCFunction)
showVectorR3 = Lua.newcfunction $ liftIO1 (show :: VectorR3 -> String)


instance Lua.StackValue VectorR3 where

  push l p = do
    pushRef l p
    __tostring <- showVectorR3
    setMetaMethod l "VectorR3" "__tostring" __tostring
    setClass l "VectorR3" "VectorR3"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA


_export_Vector :: 
  Double -> Double -> Double -> IO VectorR3
_export_Vector = liftIO3 vec3

--------------------------------------------------------------------------------
-- [Export] Color
--------------------------------------------------------------------------------

showWorldColor :: IO (FunPtr Lua.LuaCFunction)
showWorldColor = Lua.newcfunction $ liftIO1 (show :: WorldColor -> String)


instance Lua.StackValue WorldColor where

  push l c = do 
    pushRef l c
    __tostring <- showWorldColor
    setMetaMethod l "WorldColor" "__tostring" __tostring
    setClass l "WorldColor" "WorldColor"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA


_export_RGBColor :: 
  Int -> Int -> Int -> IO WorldColor 
_export_RGBColor = liftIO3 worldColor

_export_GetRed :: 
  WorldColor -> IO Int
_export_GetRed c = return (fromIntegral $ c^.r :: Int)

_export_GetGreen :: 
  WorldColor -> IO Int
_export_GetGreen c = return (fromIntegral $ c^.g :: Int)

_export_GetBlue :: 
  WorldColor -> IO Int
_export_GetBlue c = return (fromIntegral $ c^.b :: Int)

--------------------------------------------------------------------------------
-- [Export] Texture
--------------------------------------------------------------------------------

showTexture :: IO (FunPtr Lua.LuaCFunction)
showTexture = Lua.newcfunction $ liftIO1 (show :: Texture -> String)


instance Lua.StackValue Texture where

  push l c = do 
    pushRef l c
    __tostring <- showTexture
    setMetaMethod l "Texture" "__tostring" __tostring
    setClass l "Texture" "Texture"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA

_export_Pigment :: 
  WorldColor -> IO Texture
_export_Pigment = liftIO1 pigment

_export_Checkerboard :: 
  WorldColor -> WorldColor -> Double -> IO Texture
_export_Checkerboard = liftIO3 checkerboard

_export_Brick :: 
  WorldColor -> WorldColor -> IO Texture
_export_Brick = liftIO2 brick

_export_XGradient ::
  WorldColor -> WorldColor -> Double -> IO Texture
_export_XGradient = liftIO3 xgradient

_export_YGradient :: 
  WorldColor -> WorldColor -> Double -> IO Texture
_export_YGradient = liftIO3 ygradient

_export_ImageMap :: 
  FilePath -> IO Texture
_export_ImageMap = imagemap

--------------------------------------------------------------------------------
-- [Export] MappingShape
--------------------------------------------------------------------------------

showMappingShape :: IO (FunPtr Lua.LuaCFunction)
showMappingShape = Lua.newcfunction $ liftIO1 (show :: MappingShape -> String)


instance Lua.StackValue MappingShape where

  push l c = do 
    pushRef l c
    __tostring <- showMappingShape
    setMetaMethod l "MappingShape" "__tostring" __tostring
    setClass l "MappingShape" "MappingShape"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA


_export_Planar :: 
  IO MappingShape
_export_Planar = return planar

_export_Spherical :: 
  IO MappingShape
_export_Spherical = return spherical

_export_Spherical2 :: 
     Double 
  -> IO MappingShape
_export_Spherical2 = liftIO1 spherical2

_export_Cylindrical :: 
  IO MappingShape
_export_Cylindrical = return cylindrical

--------------------------------------------------------------------------------
-- [Export] MappingEntity
--------------------------------------------------------------------------------

showMappingEntity :: IO (FunPtr Lua.LuaCFunction)
showMappingEntity = Lua.newcfunction $ liftIO1 (show :: MappingEntity -> String)


instance Lua.StackValue MappingEntity where

  push l c = do 
    pushRef l c
    __tostring <- showMappingEntity
    setMetaMethod l "MappingEntity" "__tostring" __tostring
    setClass l "MappingEntity" "MappingEntity"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA

_export_HitPosition :: 
  IO MappingEntity
_export_HitPosition = return hitPosition

_export_SurfaceNormal :: 
  IO MappingEntity
_export_SurfaceNormal = return surfaceNormal

_export_FromCentroid :: 
  IO MappingEntity
_export_FromCentroid = return fromCentroid

_export_Reflection :: 
  IO MappingEntity
_export_Reflection = return reflection

--------------------------------------------------------------------------------
-- [Export] Material
--------------------------------------------------------------------------------

showMaterial :: IO (FunPtr Lua.LuaCFunction)
showMaterial = Lua.newcfunction $ liftIO1 (show :: Material -> String)


instance Lua.StackValue Material where

  push l material' = do 
    Lua.newtable l
    setField l "ambient"  $ _ambient material'
    setField l "diffuse"  $ _diffuse material'
    setField l "opacity"  $ _opacity material'
    setField l "reflect"  $ _reflect material'
    setField l "refract"  $ _refract material'
    setField l "shadow"   $ _shadow material'
    setField l "shine"    $ _shine material'
    setField l "specular" $ _specular material'
    __tostring <- showMaterial
    setMetaMethod l "Material" "__tostring" __tostring
    setClass l "Material" "Material"

  peek l i = do
    ix        <- normalizeIndex l i
    ambient'  <- getField l ix "ambient"  $ _ambient defaultMaterial
    diffuse'  <- getField l ix "diffuse"  $ _diffuse defaultMaterial
    opacity'  <- getField l ix "opacity"  $ _opacity defaultMaterial
    reflect'  <- getField l ix "reflect"  $ _reflect defaultMaterial
    refract'  <- getField l ix "refract"  $ _refract defaultMaterial
    shadow'   <- getField l ix "shadow"   $ _shadow defaultMaterial
    shine'    <- getField l ix "shine"    $ _shine defaultMaterial
    specular' <- getField l ix "specular" $ _specular defaultMaterial
    return $ Just $ Material 
      { _ambient  = ambient'
      , _diffuse  = diffuse'
      , _opacity  = opacity'
      , _reflect  = reflect'
      , _refract  = refract'
      , _shadow   = shadow'
      , _shine    = shine'
      , _specular = specular' }

  valuetype _ = Lua.TTABLE


_export_Material :: IO Material
_export_Material = return defaultMaterial

--------------------------------------------------------------------------------
-- [Export] Affine Matrix
--------------------------------------------------------------------------------

showAffineMatrix :: IO (FunPtr Lua.LuaCFunction)
showAffineMatrix = Lua.newcfunction $ liftIO1 (show :: AffineMatrix -> String)


instance Lua.StackValue AffineMatrix where

  push l m = do 
    pushRef l m
    __tostring <- showAffineMatrix
    setMetaMethod l "AffineMatrix" "__tostring" __tostring
    setClass l "AffineMatrix" "AffineMatrix"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA


_export_NewAffine :: IO AffineMatrix
_export_NewAffine = return affineId

_export_AffineTranslate :: 
  AffineMatrix -> Double -> Double -> Double -> IO AffineMatrix
_export_AffineTranslate m x y z = return $ affineTranslate x y z m

_export_AffineTranslateX :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineTranslateX m x = return $ affineTranslate x 0.0 0.0 m

_export_AffineTranslateY :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineTranslateY m y = return $ affineTranslate 0.0 y 0.0 m

_export_AffineTranslateZ :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineTranslateZ m z = return $ affineTranslate 0.0 0.0 z m

_export_AffineRotate :: 
  AffineMatrix -> Double -> Double -> Double -> IO AffineMatrix
_export_AffineRotate m x y z = return $ affineRotate x y z m

_export_AffineRotateX :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineRotateX m x = return $ affineRotate x 0.0 0.0 m

_export_AffineRotateY :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineRotateY m y = return $ affineRotate 0.0 y 0.0 m

_export_AffineRotateZ :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineRotateZ m z = return $ affineRotate 0.0 0.0 z m

_export_AffineScale :: 
  AffineMatrix -> Double -> Double -> Double -> IO AffineMatrix
_export_AffineScale m x y z = return $ affineScale x y z m

_export_AffineScaleX :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineScaleX m x = return $ affineScale x 1.0 1.0 m

_export_AffineScaleY :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineScaleY m y = return $ affineScale 1.0 y 1.0 m

_export_AffineScaleZ :: 
  AffineMatrix -> Double -> IO AffineMatrix
_export_AffineScaleZ m z = return $ affineScale 1.0 1.0 z m

performTransform :: 
     (Coordinate -> AffineMatrix) 
  -> Lua.LuaState 
  -> IO CInt
performTransform f l = do
  Just coords    <- popCoordTriple l
  maybeClassName <- readClass l $ -1
  case maybeClassName of
    Just t@"PointR3"   -> (Lua.peek l (-1) :: IO (Maybe PointR3)) >>= apply t coords
    Just t@"VectorR3"  -> (Lua.peek l (-1) :: IO (Maybe VectorR3)) >>= apply t coords
    Just t@"Camera"    -> (Lua.peek l (-1) :: IO (Maybe Camera)) >>= apply t coords
    Just t@"WorldObject:Primitive" ->
      (Lua.peek l (-1) :: IO (Maybe (WorldObject Primitive))) >>= apply t coords
    Just t -> throw $ UnsupportedType t
    _      -> throw $ MissingIndex "meta:class"
  return $ CInt 1
  where
    apply name coords maybeObj = do
      case maybeObj of
        Just obj -> do
          Lua.pop l 1
          Lua.push l $ obj <!- f coords
        Nothing  -> throw $ InvalidArgument name

_export_Translate :: Lua.LuaState -> IO CInt
_export_Translate = performTransform (\(x,y,z) -> affineTranslate x y z affineId)

_export_Rotate :: Lua.LuaState -> IO CInt
_export_Rotate = performTransform (\(x,y,z) -> affineRotate x y z affineId)

_export_Scale :: Lua.LuaState -> IO CInt
_export_Scale = performTransform (\(x,y,z) -> affineScale x y z affineId)

--------------------------------------------------------------------------------
-- [Export] Attributes
--------------------------------------------------------------------------------

showAttributes :: IO (FunPtr Lua.LuaCFunction)
showAttributes = Lua.newcfunction $ liftIO1 (show :: Attributes -> String)


instance Lua.StackValue Attributes where

  push l attrs' = do
    Lua.newtable l
    setField l "texture" $ attrs'^.surfaceTexture
    setField l "mappingShape" $ attrs'^.textureMappingShape
    setField l "mappingEntity" $ attrs'^.textureMappingEntity
    setField l "material" $ attrs'^.surfaceMaterial
    setField l "affine" $ attrs'^.affine.mBase
    __tostring <- showAttributes
    setMetaMethod l "Attributes" "__tostring" __tostring
    setClass l "Attributes" "Attributes"

  peek l i = do
    ix             <- normalizeIndex l i
    texture'       <- getField l ix "texture" $ defaultAttrs^.surfaceTexture
    mappingShape'  <- getField l ix "mappingShape" $ defaultAttrs^.textureMappingShape
    mappingEntity' <- getField l ix "mappingEntity" $ defaultAttrs^.textureMappingEntity
    material'      <- getField l ix "material" $ defaultAttrs^.surfaceMaterial
    affine'        <- getField l ix "affine" $ defaultAttrs^.affine.mBase
    return $ Just $ Attributes texture' 
                               mappingShape' 
                               mappingEntity' 
                               material' 
                               (makeTransforms affine')

  valuetype _ = Lua.TTABLE


_export_Attributes :: IO Attributes
_export_Attributes = return defaultAttrs

------------------------------------------------------------------------------
-- [Export] Primitive
------------------------------------------------------------------------------

showPrimitive :: IO (FunPtr Lua.LuaCFunction)
showPrimitive = Lua.newcfunction $ liftIO1 (show :: Primitive -> String)


instance Lua.StackValue Primitive where

  push l obj = do 
    pushRef l obj
    __tostring <- showPrimitive
    setMetaMethod l "Primitive" "__tostring" __tostring
    setClass l "Primitive" "Primitive"

  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA

--------------------------------------------------------------------------------
-- [Export] Objects
--------------------------------------------------------------------------------

showWorldObjectPrimitive :: IO (FunPtr Lua.LuaCFunction)
showWorldObjectPrimitive = 
  Lua.newcfunction $ liftIO1 (show :: WorldObject Primitive -> String)


instance (Lua.StackValue a) => Lua.StackValue (WorldObject a) where

  push l (BasicObject seqId typeName children aabb attrs') = do
    Lua.newtable l
    setField l "_tag" "BasicObject"
    setField l "seqId" seqId
    setField l "typeName" typeName
    setFieldRef l "_children" children
    setFieldRef l "_aabb" aabb
    setField l "attributes" attrs'
    __tostring <- showWorldObjectPrimitive
    setMetaMethod l "WorldObject:Primitive" "__tostring" __tostring
    setClass l "WorldObject:Primitive" "WorldObject:Primitive"

  push l (ComplexObject seqId typeName children index attrs') = do
    Lua.newtable l
    setField l "_tag" "ComplexObject"
    setField l "seqId" seqId
    setField l "typeName" typeName
    setFieldRef l "_children" children
    setFieldRef l "_index" index
    setField l "attributes" attrs'
    __tostring <- showWorldObjectPrimitive
    setMetaMethod l "WorldObject:Primitive" "__tostring" __tostring
    setClass l "WorldObject:Primitive" "WorldObject:Primitive"

  peek l i = do

    ix  <- normalizeIndex l i
    tag <- getField l ix "_tag" $ throw $ MissingArgument $ "tag"

    case tag of

      "BasicObject" -> do
        seqId'    <- getField l ix "seqId" $ throw $ MissingArgument "seqId"
        typeName' <- getField l ix "typeName" $ throw $ MissingArgument "typeName"
        children' <- getFieldRef l ix "_children" $ throw $ MissingArgument "children"
        aabb'     <- getFieldRef l ix "_aabb" $ throw $ MissingArgument "aabb"
        attrs'    <- getField l ix "attributes" $ defaultAttrs

        return $ Just $ BasicObject seqId' typeName' children' aabb' attrs'

      "ComplexObject" -> do
        seqId'    <- getField l ix "seqId" $ throw $ MissingArgument "seqId"
        typeName' <- getField l ix "typeName" $ throw $ MissingArgument "typeName"
        children' <- getFieldRef l ix "_children" $ throw $ MissingArgument "children"
        index'    <- getFieldRef l ix "_index" $ throw $ MissingArgument "index"
        attrs'    <- getField l ix "attributes" $ defaultAttrs

        return $ Just $ ComplexObject seqId' typeName' children' index' attrs'

      _ -> throw $ InvalidArgument $ "tag" ++ (show tag)

  valuetype _ = Lua.TTABLE


_export_Sphere :: 
     Counter
  -> IO (Double -> Attributes -> IO (WorldObject Primitive))
_export_Sphere counter = do
  n <- incObjectCount counter
  return $ sphere n


_export_Plane ::
     Counter
  -> IO ([Double] -> Double -> Attributes -> IO (WorldObject Primitive))
_export_Plane counter = do
  n <- incObjectCount counter
  return $ \points -> plane n $ unsafeListToTriple points


_export_Box :: 
     Counter
  -> IO ([Double] -> [Double] -> Attributes -> IO (WorldObject Primitive))
_export_Box counter = do
  n <- incObjectCount counter
  return $ \v0 v1 -> box n (unsafeListToTriple v0) (unsafeListToTriple v1)


_export_Mesh :: 
     Counter
  -> IO (FilePath -> Bool -> Attributes -> IO (WorldObject Primitive))
_export_Mesh counter = do
  n <- incObjectCount counter
  return $ mesh n

--------------------------------------------------------------------------------
-- [Export] LightSource
--------------------------------------------------------------------------------

showLightSource :: IO (FunPtr Lua.LuaCFunction)
showLightSource = Lua.newcfunction $ liftIO1 (show :: LightSource -> String)


instance Lua.StackValue LightSource where

  push l light' = do 
    pushRef l light'
    __tostring <- showLightSource
    setMetaMethod l "LightSource" "__tostring" __tostring
    setClass l "LightSource" "LightSource"
  
  peek = peekRef

  valuetype _ = Lua.TLIGHTUSERDATA


_export_PointLight :: 
     [Double]
  -> WorldColor 
  -> IO LightSource 
_export_PointLight ps c = return $ pointLight (unsafeListToTriple ps) c

--------------------------------------------------------------------------------
-- [Export] Camera
--------------------------------------------------------------------------------

showCamera :: IO (FunPtr Lua.LuaCFunction)
showCamera = Lua.newcfunction $ liftIO1 (show :: Camera -> String)


instance Lua.StackValue Camera where

  push l cam = do
    Lua.newtable l
    setField l "position" $ cam^.position
    setField l "lookAt" $ cam^.lookAt
    setField l "fov" $ cam^.fov
    setField l "up" $ cam^.up
    setField l "right" $ cam^.right
    setField l "forward" $ cam^.forward
    __tostring <- showCamera
    setMetaMethod l "Camera" "__tostring" __tostring
    setClass l "Camera" "Camera"

  peek l i = do
    ix        <- normalizeIndex l i
    position' <- getField l ix "position" $ throw $ MissingArgument "position"
    lookAt'   <- getField l ix "lookAt" $ throw $ MissingArgument "lookAt"
    fov'      <- getField l ix "fov" $ throw $ MissingArgument "fov"
    up'       <- getField l ix "up" $ throw $ MissingArgument "up"
    right'    <- getField l ix "right" $ throw $ MissingArgument "right"
    forward'  <- getField l ix "forward" $ throw $ MissingArgument "forward"
    return $ Just $ Camera { _position = position'
                           , _lookAt   = lookAt'
                           , _fov      = fov'
                           , _up       = up'
                           , _right    = right'
                           , _forward  = forward' }  

  valuetype _ = Lua.TTABLE


_export_Camera ::
     [Double] -- ^ Position: [x,y,z]
  -> [Double] -- ^ Look at: [x,y,z]
  -> Double   -- ^ Field-of-view (FOV)
  -> IO Camera
_export_Camera ps ls f = 
  return $ camera (unsafeListToTriple ps) (unsafeListToTriple ls) f

--------------------------------------------------------------------------------
-- [Export] Scene
--------------------------------------------------------------------------------

showSceneWorldObjectPrimitive :: IO (FunPtr Lua.LuaCFunction)
showSceneWorldObjectPrimitive = 
  Lua.newcfunction $ liftIO1 (show :: StdScene -> String)


instance Lua.StackValue StdScene where

  push l s = do
    Lua.newtable l
    setField l "objects" $ s^.sceneObjects
    setField l "lights" $ s^.sceneLights
    setField l "background" $ s^.sceneBackground
    __tostring <- showSceneWorldObjectPrimitive
    setMetaMethod l "Scene:WorldObject:Primitive" "__tostring" __tostring
    setClass l "Scene:WorldObject:Primitive" "Scene:WorldObject:Primitive"

  peek l i = do
    ix          <- normalizeIndex l i
    objects'    <- getField l ix "objects" $ emptyScene^.sceneObjects
    lights'     <- getField l ix "lights" $ emptyScene^.sceneLights
    background' <- getField l ix "background" $ emptyScene^.sceneBackground
    return $ Just $ Scene objects' lights' background'

  valuetype _ = Lua.TTABLE


_export_Scene :: 
     [WorldObject Primitive]
  -> [LightSource]
  -> Texture
  -> IO StdScene
_export_Scene = liftIO3 Scene 


_export_EmptyScene :: IO StdScene
_export_EmptyScene = return emptyScene

--------------------------------------------------------------------------------

-- | Initializes the given Lua state by exporting selected values and functions,
-- as well as updating the default module search path
initState :: 
     Args.RuntimeOptions -- ^ Runtime options 
  -> Lua.LuaState        -- ^ The initial Lua state instance
  -> IO Lua.LuaState
initState opts l = do
  let camLocation = Args.parse3Tuple $ opts^.Args.optLocation
  let camLookAt   = Args.parse3Tuple $ opts^.Args.optLookAt
  let defaultCam  = camera camLocation camLookAt $ opts^.Args.optFov
  counter <- newIORef 0
  Lua.openlibs l
  setPath l ["./Scenes/Lib/Lua/Include/?.lua"]
  exportGlobal l "camera" defaultCam              -- Default camera
  exportGlobal l "scene" (emptyScene :: StdScene) -- Default scene
  Lua.registerhsfunction l "Point" export_Point
  Lua.registerhsfunction l "Vector" _export_Vector
  Lua.registerhsfunction l "Camera" _export_Camera
  Lua.registerhsfunction l "RGBColor" _export_RGBColor
  Lua.registerhsfunction l "GetRed" _export_GetRed
  Lua.registerhsfunction l "GetGreen" _export_GetGreen
  Lua.registerhsfunction l "GetBlue" _export_GetBlue
  Lua.registerhsfunction l "Pigment" _export_Pigment
  Lua.registerhsfunction l "Checkerboard" _export_Checkerboard
  Lua.registerhsfunction l "Brick" _export_Brick
  Lua.registerhsfunction l "XGradient" _export_XGradient
  Lua.registerhsfunction l "YGradient" _export_YGradient
  Lua.registerhsfunction l "ImageMap" _export_ImageMap
  Lua.registerhsfunction l "Material" _export_Material
  Lua.registerhsfunction l "Planar" _export_Planar
  Lua.registerhsfunction l "Spherical" _export_Spherical
  Lua.registerhsfunction l "Spherical2" _export_Spherical2
  Lua.registerhsfunction l "Cylindrical" _export_Cylindrical
  Lua.registerhsfunction l "HitPosition" _export_HitPosition
  Lua.registerhsfunction l "SurfaceNormal" _export_SurfaceNormal
  Lua.registerhsfunction l "FromCentroid" _export_FromCentroid
  Lua.registerhsfunction l "Reflection" _export_Reflection
  Lua.registerhsfunction l "AffineTranslate" _export_AffineTranslate
  Lua.registerhsfunction l "AffineTranslateX" _export_AffineTranslateX
  Lua.registerhsfunction l "AffineTranslateY" _export_AffineTranslateY
  Lua.registerhsfunction l "AffineTranslateZ" _export_AffineTranslateZ
  Lua.registerhsfunction l "AffineRotate" _export_AffineRotate
  Lua.registerhsfunction l "AffineRotateX" _export_AffineRotateX
  Lua.registerhsfunction l "AffineRotateY" _export_AffineRotateY
  Lua.registerhsfunction l "AffineRotateZ" _export_AffineRotateZ
  Lua.registerhsfunction l "AffineScale" _export_AffineScale
  Lua.registerhsfunction l "AffineScaleX" _export_AffineScaleX
  Lua.registerhsfunction l "AffineScaleY" _export_AffineScaleY
  Lua.registerhsfunction l "AffineScaleZ" _export_AffineScaleZ
  Lua.registerhsfunction l "Attributes" _export_Attributes
  Lua.registerrawhsfunction l "Translate" _export_Translate
  Lua.registerrawhsfunction l "Rotate" _export_Rotate
  Lua.registerrawhsfunction l "Scale" _export_Scale
  Lua.registerhsfunction l "PointLight" _export_PointLight
  Lua.registerhsfunction l "Scene" _export_Scene
  Lua.registerhsfunction l "EmptyScene" _export_EmptyScene
  -- Object primitives that have counters associated with them:
  _export_Sphere counter >>= Lua.registerhsfunction l "Sphere"
  _export_Plane counter >>= Lua.registerhsfunction l "Plane"
  _export_Box counter >>= Lua.registerhsfunction l "Box"
  _export_Mesh counter >>= Lua.registerhsfunction l "Mesh"
  return l


-- | Exports the given value globally under the supplied name
exportGlobal :: 
  (Lua.StackValue a)
  => Lua.LuaState -- ^ The Lua state to modify
  -> String       -- ^ The name to export the value under
  -> a            -- ^ The value to export
  -> IO ()
exportGlobal l name value = do
  Lua.push l value
  Lua.setglobal l name


-- Reads a global value
readGlobal ::
  (Lua.StackValue a)
  => Lua.LuaState -- ^ The state to read from
  -> String       -- ^ The name of the global symbol to read
  -> IO (Maybe a)
readGlobal l name = do
  Lua.getglobal l name
  i <- Lua.gettop l
  if i > 0
    then do 
      resultIsNil <- Lua.isnil l i
      if not resultIsNil
        then do 
          output <- Lua.peek l 1
          Lua.pop l 1
          return output
        else return Nothing
    else return Nothing


-- | Sets the Lua module search path
-- Adapted from http://stackoverflow.com/a/4156038
setPath :: 
     Lua.LuaState -- ^ The Lua state to modify
  -> [FilePath]   -- ^ A list of additional paths to add to Lua's "package.path"
                  -- environment variable
  -> IO ()
setPath l paths = do
  Lua.getglobal l "package"
  Lua.getfield l (-1) "path"
  currentPath <- Lua.tostring l (-1)
  Lua.pop l 1
  Lua.push l $ currentPath ++ ";" ++ (intercalate ";" paths)
  Lua.setfield l (-2) "path"
  Lua.pop l 1


-- | Runs the given Lua scene description file
loadScene :: 
     Args.RuntimeOptions
  -> FilePath 
  -> IO (Either String (Camera,StdScene))
loadScene opts runPath = do
  l  <- Lua.newstate
  l' <- initState opts l
  catch (execMain l') handler
  where
    handler :: SomeException -> IO (Either String (Camera,StdScene))
    handler e = do
      return $ Left $ show e
    execMain l = do
      Lua.callproc l "require" "Prelude" -- Load the Prelude:
      Lua.loadfile l runPath
      statusCode <- Lua.pcall l 0 0 0
      if statusCode /= 0
        then do
          errorObj <- Lua.peek l 1 :: IO (Maybe String)
          case errorObj of
            Just errorText -> return $ Left errorText
            Nothing        -> return $ Left "???"
        else do
          maybeCamera <- readGlobal l "camera" :: IO (Maybe Camera)
          maybeScene  <- readGlobal l "scene" :: IO (Maybe StdScene)
          Lua.close l
          case (maybeCamera,maybeScene) of
            (Just c,Just s) -> return $ Right (c,s)
            _               -> return $ Left "This should never happen!!!"

