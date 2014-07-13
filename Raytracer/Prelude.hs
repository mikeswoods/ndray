{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

-- | This module serves as a convenient place to re-export all ojects that 
-- can cbe created in the context of a scene. It serves as a single
-- "one-stop-shopping" location for all objects that a user may want to 
-- instantiate

module Raytracer.Prelude (Object.WorldObject(..)
                         ,Primitive.Primitive(Sphere)
                         ,Lights.LightSource
                         ,Predefined.sphere
                         ,Predefined.plane
                         ,Predefined.box
                         ,Predefined.triangle
                         ,Predefined.mesh
                         ,Lights.pointLight
                         ,Tensor.R3(..)
                         ,Tensor.PointR3
                         ,Tensor.VectorR3
                         ,Tensor.AffineMatrix
                         ,Tensor.point3
                         ,Tensor.vec3
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
                         ,Tensor.toList
                         ,Tensor.fromList
                         ,Tensor.mBase
                         ,Tensor.mInv
                         ,Tensor.mInvT
                         ,Color.WorldColor
                         ,Color.worldColor
                         ,Color.black
                         ,Color.fuchsia
                         ,Color.r
                         ,Color.g
                         ,Color.b
                         ,Color.asByteTriple
                         ,Color.asIntTriple
                         ,Object.attributes
                         ,Object.createObject
                         ,Environment.Coordinate
                         ,Environment.Camera(..)
                         ,Environment.camera
                         ,Environment.position
                         ,Environment.lookAt
                         ,Environment.fov
                         ,Environment.up
                         ,Environment.right
                         ,Environment.forward
                         ,Texture.Texture
                         ,Texture.pigment
                         ,Texture.checkerboard
                         ,Texture.brick
                         ,Texture.xgradient
                         ,Texture.ygradient
                         ,Texture.imagemap
                         ,Mapping.MappingShape
                         ,Mapping.defaultMappingShape
                         ,Mapping.planar
                         ,Mapping.spherical
                         ,Mapping.spherical2
                         ,Mapping.cylindrical
                         ,Mapping.MappingEntity
                         ,Mapping.defaultMappingEntity
                         ,Mapping.hitPosition
                         ,Mapping.surfaceNormal
                         ,Mapping.fromCentroid
                         ,Mapping.reflection
                         ,Material.Material(..)
                         ,Material.defaultMaterial
                         ,Attributes.Attributes(..)
                         ,Attributes.defaultAttrs
                         ,Attributes.attrs
                         ,Attributes.texture
                         ,Attributes.material
                         ,Attributes.ambient
                         ,Attributes.diffuse
                         ,Attributes.opacity
                         ,Attributes.reflect
                         ,Attributes.refract
                         ,Attributes.specular
                         ,Attributes.shine
                         ,Attributes.surfaceTexture
                         ,Attributes.textureMappingShape
                         ,Attributes.textureMappingEntity
                         ,Attributes.surfaceMaterial
                         ,Attributes.affine
                         ,Scene.Scene(..)
                         ,Scene.emptyScene
                         ,Scene.sceneObjects
                         ,Scene.sceneLights
                         ,Scene.sceneBackground
                         ,Scene.object
                         ,Scene.light
                         ,Scene.background
                         ,Scene.scene)
where
import qualified Raytracer.World.Color as Color
import qualified Raytracer.World.Environment as Environment
import qualified Raytracer.Objects.Object as Object
import qualified Raytracer.Objects.Primitive as Primitive
import qualified Raytracer.Objects.Predefined as Predefined
import qualified Raytracer.World.Lights as Lights
import qualified Raytracer.Surface.Material as Material
import qualified Raytracer.Surface.Mapping as Mapping
import qualified Raytracer.Surface.Texture as Texture
import qualified Raytracer.Space.Tensor as Tensor
import qualified Raytracer.Objects.Attributes as Attributes
import qualified Raytracer.Scene.Scene as Scene
