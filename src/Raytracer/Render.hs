{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Raytracer.Render 
  (
   RGBCanvas
  ,rendertoBytesArray
  ,render'
  ,renderScene
  )
where
import Data.IORef
import System.IO.Unsafe 
  (
   unsafePerformIO
  )
import System.IO
import Data.Word 
  (
   Word8
  )
import Data.Vector.Unboxed.Base 
  (
   Unbox
  )
import Text.Printf 
  (
   printf
  )
import Control.Lens 
  (
   (^.)
  ,(.=)
  ,use
  ,makeLenses
  )
import Data.Foldable
import Control.Monad
import Control.Monad.State 
  (
   MonadState
  ,evalState
  )
import Control.Monad.Reader 
  (
   MonadReader
  ,runReaderT
  ,ask
  )
import Data.Maybe 
  (
   isJust
  ,fromJust
  )
import Data.Functor.Identity 
  (
   runIdentity
  )
import Data.Array.Repa hiding (map, (++))
import qualified Raytracer.Args as Args
import Raytracer.Utils.Array 
  (
   interpolate
  )
import Raytracer.Utils.Math 
  (
   deg2rad
  )
import Raytracer.Space.Tensor 
  (
   R3(..)
  ,AffineTransformable(..)
  ,VectorR3
  ,unit
  ,dist
  ,mag
  ,(/->)
  ,(/=>)
  ,mInvT
  )
import Raytracer.World.Color ((.*.))
import Raytracer.World.Const
import Raytracer.Objects.Object (WorldObject)
import qualified Raytracer.World.Camera as Camera
import qualified Raytracer.World.Color as Color
import qualified Raytracer.World.Intersect as Intersect
import qualified Raytracer.World.AABB as AABB
import qualified Raytracer.Objects.Attributes as Attributes
import qualified Raytracer.Surface.Texture as Texture
import qualified Raytracer.Surface.Mapping as Mapping
import Raytracer.Surface.Mapping ((.!!.))
import qualified Raytracer.Surface.Material as Material
import qualified Raytracer.Scene.Scene as Scene
import qualified Raytracer.World.Lights as Lights

--------------------------------------------------------------------------------

-- | Output canvas RGB bitmap
type RGBCanvas e = Array U DIM2 (e, e, e)

-- | Deferred ouput RGB canvas type
type DeferredRGBCanvas e = Array D DIM2 (e, e, e)

--------------------------------------------------------------------------------

putProgress :: String -> IO ()
putProgress s = hPutStr stdout $ printf "\r\ESC[K%s" s
{-# NOINLINE putProgress #-}


progressIndicator :: 
  (Num a, Integral a) 
    => IORef a 
    -> Height
    -> (forall b. (Int,Int) -> b -> b)
progressIndicator lineCount height (0,_) x = 
  unsafePerformIO $ do
    count <- readIORef lineCount
    modifyIORef' lineCount (+1)
    putProgress $ printf "%s%%" (show . ceiling $ percentage count)
    return x
  where
    percentage count = ((fromIntegral count) / (fromIntegral height)) * 100.0
progressIndicator _ _ _ x = x
{-# NOINLINE progressIndicator #-}

--------------------------------------------------------------------------------

-- | Tracks information relevant to the state of an individual pixel in
-- the rendering pipeline
data PixelState = PixelState 
  { _shadeFactor :: !Double -- The amount of shadow shading to apply to the pixel
                            -- Currently this value is either 0.0 or 1.0
  }

makeLenses ''PixelState


-- Creates a new pixel state with a shade factor of 1.0, e.g. unshaded 
initPixelState :: PixelState
initPixelState = PixelState 
  { _shadeFactor  = 1.0 }

--------------------------------------------------------------------------------

-- | Computes the direction of a reflection ray
calcReflectionDir ::   
     VectorR3 -- ^ The surface normal
  -> VectorR3 -- ^ The incident (surface-to-light) vector
  -> VectorR3
calcReflectionDir !n !i = unit $ i - (n *. (2 * (i |. n)))
{-# INLINE calcReflectionDir #-}


-- | Computes the direction of a refraction ray
calcRefractionDir ::   VectorR3        -- ^ The surface normal
                    -> VectorR3        -- ^ The direction vector
                    -> (Double,Double) -- ^ the index of refraction (IOR)
                                       -- for the (outer,inner) materials
                    -> VectorR3
calcRefractionDir !n !d !(outerIOR,innerIOR)
  | coeff > 0.0 = (d *. ior) + (n *. ((ior * d') - sqrt coeff))
  | otherwise   = d
  where 
    ior   = outerIOR / innerIOR
    d'    = -(n |. d)
    coeff = 1.0 - (ior * ior) * (1.0 - (d' * d'))
{-# INLINE calcRefractionDir #-}

--------------------------------------------------------------------------------

-- | Simple test to determine if a ray intersects the given bounded container.
-- | If the ray does intersect, the color assigned to the maximal bounding box
-- | for all of the child objects in container will be returned.
drawMaxAABB ::
  (AABB.WithAABB a
  ,Attributes.WithAttributes a
  ,Intersect.Intersectable a)
  => Intersect.Ray -- ^ The ray to test for intersection
  -> [a]           -- ^ The bounding container to test for intersection
  -> Maybe Color.WorldColor 
drawMaxAABB !ray !allBounded = 
  case intersectedBounded of
    [] -> Nothing
    _  -> Just $ Color.lime
  where
    o = ray^.Intersect.origin
    d = ray^.Intersect.direction
    intersectedBounded =
      flip filter allBounded $ \b -> do
        AABB.hits o d (AABB.boundingBoxOf b)


-- | Using the given options, camera and scene, render the output to a
-- | RGB bitmap
rendertoBytesArray :: 
     Args.RuntimeOptions
  -> Camera.Camera
  -> Scene.Scene
  -> IO (RGBCanvas Word8)
rendertoBytesArray = render' Color.toBytes


-- | Renders the given scene, producing a REPA array of image data
-- | using a custom (WorldColor -> a) output function
render' :: 
  (Unbox e)
  => (Color.WorldColor -> e) -- ^
  -> Args.RuntimeOptions     -- ^ Runtime render configuration options
  -> Camera.Camera           -- ^ The virtual camera to render with
  -> Scene.Scene             -- ^ The scene description to render
  -> IO (Array U DIM2 e)
render' !output !opts !cam !scene = do
  totalLines <- newIORef 1
  bitmapCanvas <- renderScene opts cam scene 0 output $ progressIndicator totalLines h
  return $ runIdentity $ computeUnboxedP bitmapCanvas
  where
    h  = opts^.Args.optHeight
    bs = opts^.Args.optBlockSize
    runInterpolation arr
      | bs == 1   = delay arr
      | otherwise = interpolate bs $ arr


renderScene :: 
  (Monad m)
  => Args.RuntimeOptions
  -> Camera.Camera
  -> Scene.Scene
  -> Int
  -> (Color.WorldColor -> a)
  -> ((Int, Int) -> Color.WorldColor -> Color.WorldColor)
  -> m (Array D DIM2 a)
renderScene !opts !cam !scene !depth !makeColor !progress = do
  return $ fromFunction (ix2 height width) pixelByteValue
  where
    width       = opts^.Args.optWidth
    height      = opts^.Args.optHeight
    width'      = fromIntegral width  :: Double
    height'     = fromIntegral height :: Double
    aspectRatio = width' / height'
    angle       = tan $ deg2rad $ (cam^.Camera.fov) * 0.5
    angAspect   = aspectRatio * angle

    pixelByteValue !(Z :.j :.i) = makeColor $ drawPixel (i,height - j)

    drawPixel !at = 
      let g = if opts^.Args.optQuiet then id else progress at
      in  g $ samplePixel (shootRay at) at

    shootRay !(i,j) = Intersect.createRay p $ -rayDir
      where
        p   = cam^.Camera.position 
        i'  = ((fromIntegral i :: Double) / width') - 0.5
        j'  = ((fromIntegral j :: Double) / height') - 0.5
        rayDir  = cam^.Camera.forward 
                  + ((cam^.Camera.right) *. (i' * angAspect)) 
                  + ((cam^.Camera.up) *. (j' * angle))

    samplePixel' withRay screenXY =
      rayTrace scene withRay depth screenXY

    samplePixel withRay screenXY =
      evalState (runReaderT (samplePixel' withRay screenXY) opts) initPixelState


-- | The "core" of the raytracer algorithm
rayTrace ::
  (MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m)
  => Scene.Scene
  -> Intersect.Ray
  -> Int
  -> (Int, Int)
  -> m Color.WorldColor
rayTrace !scene !primaryRay !depth !(i,j) = do
  opts <- ask
  case (Intersect.closestSceneIntersection primaryRay $ scene^.Scene.sceneObjects) of
    Just (obj,hit) -> 
      computeRefraction scene obj depth (i,j) hit .
      computeReflection scene obj depth (i,j) hit .
      computeShading scene obj hit .
      computeShadows scene obj hit . 
      computeObjectTexture obj $
      hit
    Nothing  -> 
      if (opts^.Args.optBounding)
        then case hitAABB of
               Just aabbColor -> return aabbColor
               Nothing        -> bgColor
        else bgColor
  where
    hitAABB = drawMaxAABB primaryRay $ scene^.Scene.sceneObjects
    bgColor = computeWorldTexture (i,j) $ scene^.Scene.sceneEnvironment


-- | Calculates the color for the given ray at the point of intersection 
-- | with the given scene object 
-- | See for UV mapping/texturing details:
-- | http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_6_Textures_Cameras_and_Speed.shtml
computeObjectTexture :: 
  (MonadReader Args.RuntimeOptions m
  ,Attributes.WithAttributes a
  ,AABB.WithAABB a)
  => a
  -> Intersect.RayHit
  -> m Color.WorldColor
computeObjectTexture !obj !hit = 
  return $ Texture.texel (m .!!. (uv^.Texture.face)) (uv^.Texture.position)
  where
    attrs   = Attributes.attributesOf obj
    aabb    = AABB.boundingBoxOf obj
    m       = attrs^.Attributes.surfaceMapping
    p       = hit^.Intersect.hitAtLocal
    n       = (hit^.Intersect.normal) <!- (attrs^.Attributes.affine.mInvT)
    c       = AABB.centroidOf obj
    cv      = c /=> p
    (x,y,z) = case (m^.Mapping.entityType) of
      Mapping.Position   -> (getX p,getY p,getZ p)
      Mapping.Normal     -> (getX n,getY n,getZ n)
      Mapping.Centroid   -> (getX cv,getY cv,getZ cv)
      Mapping.Reflection -> undefined
    uv      = Mapping.findUV m aabb (x,y,z)
{-# INLINE computeObjectTexture #-}


-- | Calculates the color in the event of a ray miss
computeWorldTexture :: 
    (MonadState PixelState m
    ,MonadReader Args.RuntimeOptions m
    ,Texture.WithTexture t)
    => (Int, Int)
    -> t
    -> m Color.WorldColor
computeWorldTexture !(i,j) !bgTexture = do
  opts' <- ask 
  return $ Texture.texel bgTexture (u opts',v opts')
  where
    u opts = (fromIntegral i :: Double) / 
               (fromIntegral $ opts^.Args.optWidth :: Double)
    v  opts = (fromIntegral j :: Double) / 
               (fromIntegral $ opts^.Args.optHeight :: Double)
{-# INLINE computeWorldTexture #-}


-- | Applies shading; see 
-- | http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
-- | http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_2_Phong_Mirrors_and_Shadows.shtml
computeShading ::
  (MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m)
  => Scene.Scene
  -> WorldObject
  -> Intersect.RayHit
  -> m Color.WorldColor 
  -> m Color.WorldColor
computeShading !scene !obj !hit !color = do
  opts' <- ask
  if opts'^.Args.optShading == 0 
    then color
    else do
      c     <- color
      shade <- use shadeFactor
      -- If the object's "shadow" material attribute is false, then
      -- no shadow can be cast onto it as well
      let shade' = if material^.Material.shadow then shade else 1.0
      return $ fold $ do
        light <- scene^.Scene.sceneLights
        let lightColor                    = light^.Lights.color
        let (intensity, diffAmt, specAmt) = factors light
        let aAmt = computeAmbient c
        let dAmt = computeDiffuse c lightColor shade' intensity diffAmt
        let sAmt = computeSpecular lightColor shade' intensity specAmt
        return $ aAmt + dAmt + sAmt

  where
    attrs     = Attributes.attributesOf obj
    material  = attrs^.Attributes.surfaceMaterial
    shine     = floor $ material^.Material.shine
    p         = hit^.Intersect.hitAt
    n         = hit^.Intersect.normal

    factors light =
      let lightDir = p /-> (light^.Lights.position)
          aAmt     = attenuation lightDir
          dAmt     = diffuseAmt lightDir
          sAmt     = specularAmt lightDir
      in  (aAmt, dAmt, sAmt)

    attenuation v = 1.0 / (mag v) ^ 2
    diffuseAmt v  = max 0 $ n |. v
    specularAmt v = max 0 $ v |. (-(calcReflectionDir n v))

    computeAmbient baseColor =
      baseColor .*. (material^.Material.ambient)

    computeDiffuse baseColor lightColor shade intensity amount
      | (material^.Material.diffuse) > 0 && intensity > 0 && shade > 0 =
          (baseColor * lightColor) .*.
          (shade * intensity * (material^.Material.diffuse) * amount)
      | otherwise = Color.black
    
    computeSpecular lightColor shade intensity amount
      | (material^.Material.specular) > 0 && intensity > 0 && shade > 0 =
          lightColor .*. 
          (shade * intensity * (material^.Material.specular) * (amount ^ shine))
      | otherwise = Color.black
{-# INLINE computeShading #-}


-- | Computes reflection rays from the given ray/object intersection
computeReflection ::
  (MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m)
  => Scene.Scene
  -> WorldObject
  -> Int
  -> (Int, Int)
  -> Intersect.RayHit
  -> m Color.WorldColor
  -> m Color.WorldColor
computeReflection !scene !obj !depth !(i,j) !hit !color = do
  opts' <- ask
  if opts'^.Args.optReflect == 0 
    then color
    else compute opts'
  where
    compute opts
      | (material^.Material.reflect) > 0.0 && depth < (opts^.Args.optMaxDepth) = do
          color' <- color
          let reflDir = calcReflectionDir n d
          let reflRay = Intersect.bump $ Intersect.createRay p reflDir
          reflColor <- rayTrace scene reflRay (depth + 1) (i,j)
          return $ (reflColor .*. (material^.Material.reflect)) + color'
      | otherwise = color
      where
        attrs    = Attributes.attributesOf obj
        material = attrs^.Attributes.surfaceMaterial
        p        = hit^.Intersect.hitAt
        d        = hit^.Intersect.ray.Intersect.direction
        n        = hit^.Intersect.normal
{-# INLINE computeReflection #-}


-- | Computes refraction rays from the given ray/object intersection
computeRefraction ::
  (MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m)
  => Scene.Scene
  -> WorldObject
  -> Int
  -> (Int, Int)
  -> Intersect.RayHit
  -> m Color.WorldColor
  -> m Color.WorldColor
computeRefraction !scene !obj !depth !(i,j) !hit !color = do
  opts' <- ask
  if opts'^.Args.optRefract == 0 
    then color
    else compute opts'
  where
    compute opts
      | (material^.Material.opacity) > 0.0 && depth < (opts^.Args.optMaxDepth) = do
          color' <- color
          let refrRay = Intersect.bump $ Intersect.createRay p d'
          refrColor <- rayTrace scene refrRay (depth + 1) (i,j)
          return $ (color' .*. oAmt) + (refrColor .*. (1.0 - oAmt))
      | otherwise  = color
      where
        attrs    = Attributes.attributesOf obj
        material = attrs^.Attributes.surfaceMaterial
        rAmt     = material^.Material.refract
        oAmt     = material^.Material.opacity
        p        = hit^.Intersect.hitAt
        n        = hit^.Intersect.normal
        d        = hit^.Intersect.ray.Intersect.direction
        d'       = if rAmt /= 1.0 then calcRefractionDir n d (1.0,rAmt) else d
{-# INLINE computeRefraction #-}


-- | Computes  basic "hard" shadows
computeShadows ::
  (MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m)
  => Scene.Scene
  -> WorldObject
  -> Intersect.RayHit
  -> m Color.WorldColor
  -> m Color.WorldColor
computeShadows !scene !hitObj !hit !color = do
  opts <- ask
  if opts^.Args.optShadows == 0 
    then color
    else do
      shadeFactor .= factor
      color
  where
    p            = hit^.Intersect.hitAt
    lights       = scene^.Scene.sceneLights
    lightCount   = fromIntegral $ length lights :: Double
    hasShadow    = Attributes.surfaceMaterial.Material.shadow
    withShadow o = (Attributes.attributesOf o)^.hasShadow
    objs         = filter withShadow $ scene^.Scene.sceneObjects
    closestHits = do
      light <- lights
      let lightDir = (light^.Lights.position) /=> p
      let lightRay = Intersect.bump $ Intersect.createRay p lightDir
      let hit'     = Intersect.closestSceneIntersection lightRay objs
      guard $ isJust hit'
      return $ (fromJust hit',light)
    hitTest ((o,h),l) = 
      -- Requirements:
      -- > Did not intersect with itself
      -- > The object the shadow ray intersected is not further 
      --   than the light source that generated the shadow ray 
      (o /= hitObj) && h^.Intersect.dist <= (dist (l^.Lights.position) p)
    obstructions     = length $ filter hitTest closestHits
    obstructionCount = fromIntegral obstructions :: Double
    factor           = abs ((lightCount - obstructionCount) / lightCount)
{-# INLINE computeShadows #-}
