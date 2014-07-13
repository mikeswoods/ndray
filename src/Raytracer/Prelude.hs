{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

-- | This module serves as a convenient place to re-export all ojects that 
-- | can cbe created in the context of a scene. It serves as a single
-- | "one-stop-shopping" location for all objects that a user may want to 
-- | instantiate
module Raytracer.Prelude 
  (
   Object.WorldObject
  ,Object.PrimBox
  ,Object.BoundingVolume
  ,Primitive.Primitive(Sphere)
  ,Lights.LightSource
  ,Predefined.sphere
  ,Predefined.plane
  ,Predefined.box
  ,Predefined.triangle
  ,Predefined.mesh
  ,Lights.pointLight
  ,Tensor.R3(..)
  ,Tensor.PointR3(..)
  ,Tensor.VectorR3(..)
  ,Tensor.AffineMatrix
  ,Tensor.point3
  ,Tensor.vec3
  ,Tensor.zeroPoint
  ,Tensor.zeroVec
  ,Tensor.affineId
  ,Tensor.affineTranslate
  ,Tensor.affineRotate
  ,Tensor.affineScale
  ,Tensor.AffineTransformable(..)
  ,Tensor.makeTransforms
  ,Tensor.applyTransforms
  ,Tensor.translate
  ,Tensor.translateX
  ,Tensor.translateY
  ,Tensor.translateZ
  ,Tensor.rotate
  ,Tensor.rotateX
  ,Tensor.rotateY
  ,Tensor.rotateZ
  ,Tensor.scale
  ,Tensor.scaleX
  ,Tensor.scaleY
  ,Tensor.scaleZ
  ,Tensor.mBase
  ,Tensor.mInv
  ,Tensor.mInvT
  ,Color.WorldColor
  ,Color.worldColor
  ,Color.black
  ,Color.white
  ,Color.fuchsia
  ,Color.r
  ,Color.g
  ,Color.b
  ,Color.toBytes
  ,Color.toInts
  ,Object.attributes
  ,Object.createObject
  ,Const.R2Coord
  ,Const.R3Coord
  ,Camera.Camera
  ,Camera.camera
  ,Camera.defaultCamera
  ,Camera.position
  ,Camera.lookAt
  ,Camera.fov
  ,Texture.Texture(..)
  ,Texture.WithTexture(..)
  ,BasicTexture.BasicTexture
  ,BasicTexture.pigment
  ,BasicTexture.checkerboard
  ,BasicTexture.alt
  ,BasicTexture.brick
  ,BasicTexture.sine  , BasicTexture.SineOpts(..)  , BasicTexture.defaultSineOpts
  ,BasicTexture.xgradient
  ,BasicTexture.ygradient
  ,ImageTexture.ImageTexture
  ,ImageTexture.imagemap
  ,Mapping.usingPosition
  ,Mapping.usingNormal
  ,Mapping.usingCentroid
  ,Mapping.usingReflection
  ,Mapping.mapToX
  ,Mapping.mapToY
  ,Mapping.mapToZ
  ,Mapping.mapToSphere
  ,Mapping.mapToCylinder
  ,Mapping.mapToCube
  ,Material.Material
  ,Material.defaultMaterial
  ,Attributes.Attributes
  ,Attributes.defaultAttrs
  ,Attributes.attrs
  ,Attributes.textureMap
  ,Attributes.material
  ,Attributes.ambient
  ,Attributes.diffuse
  ,Attributes.opacity
  ,Attributes.reflect
  ,Attributes.refract
  ,Attributes.specular
  ,Attributes.shine
  ,Attributes.surfaceMapping
  ,Attributes.surfaceMaterial
  ,Attributes.affine
  ,Scene.Scene(..)
  ,Scene.emptyScene
  ,Scene.sceneObjects
  ,Scene.sceneLights
  ,Scene.sceneEnvironment
  ,Scene.object
  ,Scene.light
  ,Scene.environment
  ,Scene.scene
  )
where
import qualified Raytracer.World.Camera as Camera
import qualified Raytracer.World.Color as Color
import qualified Raytracer.World.Const as Const
import qualified Raytracer.Objects.Object as Object
import qualified Raytracer.Objects.Primitive as Primitive
import qualified Raytracer.Objects.Predefined as Predefined
import qualified Raytracer.World.Lights as Lights
import qualified Raytracer.Surface.Material as Material
import qualified Raytracer.Surface.Mapping as Mapping
import qualified Raytracer.Surface.Texture as Texture
import qualified Raytracer.Surface.Texture.Basic as BasicTexture
import qualified Raytracer.Surface.Texture.Image as ImageTexture
import qualified Raytracer.Space.Tensor as Tensor
import qualified Raytracer.Objects.Attributes as Attributes
import qualified Raytracer.Scene.Scene as Scene
