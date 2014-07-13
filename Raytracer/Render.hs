{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Raytracer.Render (render, renderScene, Canvas)
where
import System.IO.Unsafe (unsafePerformIO)
import GHC.Word (Word8)
import Text.Printf (printf)
import Control.Lens ((^.), (.=), use, makeLenses)
import Data.List(foldl')
import Control.Monad.State (MonadState, runState)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import Data.Maybe (isJust, fromJust)
import Data.Functor.Identity (runIdentity)
import Data.Array.Repa hiding (map)

import qualified Raytracer.Args as Args
import Raytracer.Utils.Array (interpolate)
import Raytracer.Utils.Math (deg2rad)
import Raytracer.Space.Tensor (R3(..), AffineTransformable(..)
  ,VectorR3, unit, dist, mag, (/->), mInvT)
import Raytracer.World.Color ((.*.))
import qualified Raytracer.World.Color as C
import qualified Raytracer.World.Environment as E
import qualified Raytracer.World.Intersect as I
import qualified Raytracer.World.AABB as AABB
import qualified Raytracer.Objects.Attributes as A
import qualified Raytracer.Surface.Texture as T
import qualified Raytracer.Surface.Mapping as Mapping
import qualified Raytracer.Surface.Material as M
import qualified Raytracer.Scene.Scene as S
import qualified Raytracer.World.Lights as L

--------------------------------------------------------------------------------

type Canvas = Array D DIM2 (Word8, Word8, Word8)

type ProgressFunc = forall a. (Int,Int) -> Int -> a -> a

--------------------------------------------------------------------------------

updateSceneStats :: (Int,Int) -> Int -> a -> a
updateSceneStats !(i,j) _
  | i == 0 = 
      unsafePerformIO $ do
        if j == 1 
          then putStrLn $ printf "%d.. Done\n" j
          else putStr   $ printf "%d.. " j
        return id
  | otherwise = id

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
calcReflectionDir ::   VectorR3 -- ^ The surface normal
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


-- | Simple test to determine if a ray intersects the given bounded container.
-- If the ray does intersect, the color assigned to the maximal bounding box
-- for all of the child objects in container will be returned.
drawMaxAABB ::
  (AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a)
    => I.Ray              -- ^ The ray to test for intersection
    -> [container a]      -- ^ The bounding container to test for intersection
    -> Maybe C.WorldColor 
drawMaxAABB !ray !allBounded = 
  case intersectedBounded of
    [] -> Nothing
    _  -> Just $ C.lime
  where
    o = ray^.I.origin
    d = ray^.I.direction
    intersectedBounded =
      flip filter allBounded $ \b -> do
        AABB.hits o d (AABB.aabb b)


-- | Renders the given scene, producing a REPA array of image data
render ::
  (Eq (container a)
  ,AABB.WithAABB a
  ,AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a) 
  => Args.RuntimeOptions
  -> E.Camera
  -> S.Scene (container a)
  -> IO (Array U DIM2 (Word8, Word8, Word8))
render !opts !cam !scene = do
  canvas <- renderScene opts cam scene 0 updateSceneStats
  return $ runInterpolation $ runIdentity $ computeUnboxedP canvas
  where
    bs = opts^.Args.optBlockSize
    runInterpolation arr
      | bs == 1   = arr
      | otherwise = computeUnboxedS $ interpolate bs $ arr


-- | Renders the given scene using the supplied dimensions, camera, and
-- scene instance
renderScene ::
  (Eq (container a)
  ,AABB.WithAABB a
  ,AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a)
  => Args.RuntimeOptions
  -> E.Camera
  -> S.Scene (container a)
  -> Int
  -> ProgressFunc
  -> IO (Array D DIM2 (Word8, Word8, Word8))
renderScene !opts !cam !scene !depth !progress = do
    return $ fromFunction (ix2 height width) pixelByteValue
  where
    width       = opts^.Args.optWidth
    height      = opts^.Args.optHeight
    width'      = fromIntegral width  :: Double
    height'     = fromIntegral height :: Double
    aspectRatio = width' / height'
    angle       = tan $ deg2rad $ (cam^.E.fov) * 0.5
    angAspect   = aspectRatio * angle

    --pixelByteValue :: ((Z :. Int) :. Int) -> (Word8, Word8, Word8)
    pixelByteValue !(Z :.j :.i) = do
      C.asByteTriple $ drawPixel (i,height - j)

    --drawPixel :: (Int,Int) -> C.WorldColor
    drawPixel !coordinates = do
      let intermediate = runReaderT (rayTrace scene ray depth coordinates) opts
      let (pixelColor, _) = runState intermediate initPixelState
      printStatus pixelColor
      where
        ray = shootRayFrom coordinates
        printStatus
          | (opts^.Args.optQuiet) = id 
          | otherwise             =  progress coordinates height

    shootRayFrom !(i,j) = I.createRay p $ -rd
      where
        p   = cam^.E.position 
        i'  = ((fromIntegral i :: Double) / width') - 0.5
        j'  = ((fromIntegral j :: Double) / height') - 0.5
        rd  = cam^.E.forward 
              + ((cam^.E.right) *. (i' * angAspect)) 
              + ((cam^.E.up) *. (j' * angle))


-- | ===========================================================================
-- | The "core" of the raytracer algorithm
-- | ===========================================================================
rayTrace ::
  (Eq (container a)
  ,MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m
  ,AABB.WithAABB a
  ,AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a)
  => S.Scene (container a)
  -> I.Ray
  -> Int
  -> (Int, Int)
  -> m C.WorldColor
rayTrace !scene !primaryRay !depth !(i,j) = do
  opts <- ask
  case (I.closestSceneIntersection primaryRay $ scene^.S.sceneObjects) of
    Just (boundedObj, hit) -> 
      let attrs = A.getAttributes boundedObj
      in  computeRefraction scene depth (i,j) attrs hit .
          computeReflection scene depth (i,j) attrs hit .
          computeShading scene attrs hit .
          computeShadows scene attrs hit . 
          computeObjectTexture attrs $
          hit
    Nothing  -> if (opts^.Args.optBounding)
                then case hitAABB of
                       Just aabbColor -> return aabbColor
                       Nothing        -> bgColor
                else bgColor
  where
    hitAABB = drawMaxAABB primaryRay $ scene^.S.sceneObjects
    bgColor = computeBackgroundTexture (i,j) $ scene^.S.sceneBackground

-- | ===========================================================================
-- | Calculates the color for the given ray at the point of intersection 
-- | with the given scene object 
-- | See for UV mapping/texturing details:
-- | http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_6_Textures_Cameras_and_Speed.shtml
-- | ===========================================================================
computeObjectTexture :: 
  (MonadReader Args.RuntimeOptions m)
    => A.Attributes
    -> I.RayHit a b
    -> m C.WorldColor
computeObjectTexture !attrs !hit = 
  return $ T.texel t $ Mapping.uv (attrs^.A.textureMappingShape) (x,y,z)
  where 
    t       = attrs^.A.surfaceTexture
    p       = hit^.I.hitAtLocal
    n       = (hit^.I.normal) <!- (attrs^.A.affine.mInvT)
    (x,y,z) = 
      case (attrs^.A.textureMappingEntity) of
        Mapping.HitPosition   -> (getX p,getY p,getZ p)
        Mapping.SurfaceNormal -> (getX n,getY n,getZ n)
        Mapping.FromCentroid  -> undefined
        Mapping.Reflection    -> undefined
{-# INLINE computeObjectTexture #-}


-- | ===========================================================================
-- | Calculates the color in the event of a ray miss
-- | ===========================================================================
computeBackgroundTexture :: 
    (MonadState PixelState m
    ,MonadReader Args.RuntimeOptions m)
    => (Int, Int)
    -> T.Texture
    -> m C.WorldColor
computeBackgroundTexture !(i,j) !bgTexture = do
  opts' <- ask 
  return $ T.getTexel bgTexture (u opts',v opts')
  where
    u opts = (fromIntegral i :: Double) / 
               (fromIntegral $ opts^.Args.optWidth :: Double)
    v  opts = (fromIntegral j :: Double) / 
               (fromIntegral $ opts^.Args.optHeight :: Double)

{-# INLINE computeBackgroundTexture #-}


-- | ===========================================================================
-- | Applies shading; see 
-- | http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
-- | http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_2_Phong_Mirrors_and_Shadows.shtml
-- | ===========================================================================
computeShading ::
  (MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m
  ,AABB.WithAABB a
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a)
  => S.Scene (container a)
  -> A.Attributes
  -> I.RayHit (container a) a
  -> m C.WorldColor 
  -> m C.WorldColor
computeShading !scene !attrs !hit !color = do
  opts' <- ask
  if opts'^.Args.optShading == 0 
    then color
    else do
      c     <- color
      shade <- use shadeFactor -- use pulls the "shadeFactor" value from the
                               -- current MonadState PixelState instance 
      return $ foldl' (+) C.black $
        [(calcAmbient c) +
         (calcDiffuse c (l^.L.color) shade intensity diffAmt) +
         (calcSpecular (l^.L.color) shade intensity specAmt)
           | (l, intensity, diffAmt, specAmt) <- factors]

  where
    material  = attrs^.A.surfaceMaterial
    shine     = floor $ material^.M.shine

    p         = hit^.I.hitAt
    n         = hit^.I.normal

    lightDirs = [(l,p /-> (l^.L.position)) | l <- (scene^.S.sceneLights)]

    factors   = [(l,attenuation v,diffuseAmt v,specularAmt v) | (l,v) <- lightDirs]

    attenuation v = 1.0 / (mag v) ^ 2

    diffuseAmt v = max 0 $ n |. v

    specularAmt v = max 0 $ v |. (-(calcReflectionDir n v))

    calcAmbient baseColor =
      baseColor .*. (material^.M.ambient)

    calcDiffuse baseColor lightColor shade intensity amount
      | (material^.M.diffuse) > 0 && intensity > 0 && shade > 0 =
          (baseColor * lightColor) .*. (shade * intensity * (material^.M.diffuse) * amount)
      | otherwise = C.black
    
    calcSpecular lightColor shade intensity amount
      | (material^.M.specular) > 0 && intensity > 0 && shade > 0 =
          lightColor .*. (shade * intensity * (material^.M.specular) * (amount ^ shine))
      | otherwise = C.black

{-# INLINE computeShading #-}


-- | ===========================================================================
-- | Computes reflection rays from the given ray/object intersection
-- | ===========================================================================
computeReflection ::
  (Eq (container a)
  ,MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m
  ,AABB.WithAABB a
  ,AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a)
  => S.Scene (container a)
  -> Int
  -> (Int, Int)
  -> A.Attributes
  -> I.RayHit (container a) a
  -> m C.WorldColor
  -> m C.WorldColor
computeReflection !scene !depth !(i,j) !attrs !hit !color = do
  opts' <- ask
  if opts'^.Args.optReflect == 0 
    then color
    else compute opts'
  where
    compute opts
      | (material^.M.reflect) > 0.0 && depth < (opts^.Args.optMaxDepth) = do
          color' <- color
          let reflDir = calcReflectionDir n d
          let reflRay = I.bump $ I.createRay p reflDir
          reflColor <- rayTrace scene reflRay (depth + 1) (i,j)
          return $ (reflColor .*. (material^.M.reflect)) + color'
      | otherwise = color
      where
        
        material = attrs^.A.surfaceMaterial
       
        p        = hit^.I.hitAt
        d        = hit^.I.ray.I.direction
        n        = hit^.I.normal

{-# INLINE computeReflection #-}


-- | ===========================================================================
-- | Computes refraction rays from the given ray/object intersection
-- | ===========================================================================
computeRefraction ::
  (Eq (container a)
  ,MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m
  ,AABB.WithAABB a
  ,AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a) 
  => S.Scene (container a)
  -> Int
  -> (Int, Int)
  -> A.Attributes
  -> I.RayHit (container a) a
  -> m C.WorldColor
  -> m C.WorldColor
computeRefraction !scene !depth !(i,j) !attrs !hit !color = do
  opts' <- ask
  if opts'^.Args.optRefract == 0 
    then color
    else compute opts'
  where
    compute opts
      | (material^.M.opacity) > 0.0 && depth < (opts^.Args.optMaxDepth) = do
          color' <- color
          let refrRay = I.bump $ I.createRay p d'
          refrColor <- rayTrace scene refrRay (depth + 1) (i,j)
          return $ (color' .*. oAmt) + (refrColor .*. (1.0 - oAmt))
      | otherwise  = color
      where

        material = attrs^.A.surfaceMaterial
        rAmt     = material^.M.refract
        oAmt     = material^.M.opacity

        p        = hit^.I.hitAt
        n        = hit^.I.normal
        d        = hit^.I.ray.I.direction

        d'       = if rAmt /= 1.0 then calcRefractionDir n d (1.0,rAmt) else d

{-# INLINE computeRefraction #-}


-- | ===========================================================================
-- | Computes  basic "hard" shadows
-- | ===========================================================================
computeShadows ::
  (Eq (container a)
  ,MonadState PixelState m
  ,MonadReader Args.RuntimeOptions m
  ,AABB.WithAABB a
  ,AABB.WithAABB (container a)
  ,A.WithAttributes (container a)
  ,I.Intersectable (container a) a)
  => S.Scene (container a)
  -> A.Attributes
  -> I.RayHit (container a) a
  -> m C.WorldColor
  -> m C.WorldColor
computeShadows !scene _ !hit !color = do
  opts <- ask
  if opts^.Args.optShadows == 0 
    then color
    else do
      if shadowHitCount > 0
         then shadeFactor .= 0.0
         else return ()
      color
  where 
    p             = hit^.I.hitAt
    raysLights    = [(I.bump $ I.createRay p $ (l^.L.position) /-> p,l) | l <- (scene^.S.sceneLights)]
    objs          = [obj | obj <- scene^.S.sceneObjects, (A.getAttributes obj)^.A.surfaceMaterial.M.shadow]
    candidateHits = [(I.closestSceneIntersection r objs,l) | (r,l) <- raysLights]
    closestHits   = [(snd . fromJust $ h,l) | (h,l) <- candidateHits, isJust h]
    hitTest (h,l) = -- Requirements:
                    -- > Did not intersect with itself
                     (h^.I.intersectedParent /= hit^.I.intersectedParent) && 
                    -- > The object the shadow ray intersected is not further 
                    --   than the light source that generated the shadow ray 
                     h^.I.intersectedDist <= (dist (l^.L.position) p)
    shadowHitCount = length $ filter hitTest closestHits

{-# INLINE computeShadows #-}
