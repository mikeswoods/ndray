module Scenes.Lib.Demo.Mesh
where
import Raytracer.Prelude
import Raytracer.World.Color

--------------------------------------------------------------------------------

checkerFloor :: WorldObject Primitive
checkerFloor = 
  plane (0.0, 1.0, 0.0) 1.0 $
    attrs $ do
      texture $ checkerboard white black 1
      ambient 0.4
      diffuse 0.6

--------------------------------------------------------------------------------

square :: IO (Scene (WorldObject Primitive))
square = do
  squareMesh <- mesh "Models/square.obj" False $
                  attrs $ do
                    texture $ pigment lime
  return $ 
    scene $ do
        background $ pigment black
        light $
          pointLight (1.0, 4.0, -2.0) white
        object squareMesh
        object checkerFloor

--------------------------------------------------------------------------------

diamond :: IO (Scene (WorldObject Primitive))
diamond = do
  diamondMesh <- mesh "Models/diamond.obj" False $
                   attrs $ do
                     texture $ pigment red
  return $ 
    scene $ do
        background $ pigment black
        light $
          pointLight (1.0, 4.0, -2.0) white
        object diamondMesh
        object checkerFloor

--------------------------------------------------------------------------------

cube :: IO (Scene (WorldObject Primitive))
cube = do
  cubeMesh <- mesh "Models/cube.obj" False $
                attrs $ do
                  texture $ pigment red
  return $ 
    scene $ do
      background $ pigment black
      light $
        pointLight (1.0, 4.0, -2.0) white
      object cubeMesh
      object checkerFloor

--------------------------------------------------------------------------------

icosahedron :: IO (Scene (WorldObject Primitive))
icosahedron = do 
  icosahedronMesh <- mesh "Models/icosahedron.obj" False $
                       attrs $ do
                         texture $ pigment blue
                         diffuse 0.9
                         specular 0.9
                         ambient 0.1
                         reflect 0.3
                         shine 50
  sky003 <- imagemap "Sky/001"
  return $ 
    scene $ do
      background $ sky003
      light $
        pointLight (5, 10.0, -1.0) white
      object $ do { rotateX 45; rotateY 45 } =!> icosahedronMesh
      object checkerFloor

--------------------------------------------------------------------------------

octahedron :: IO (Scene (WorldObject Primitive))
octahedron = do
  octahedronMesh <- mesh "Models/octahedron.obj" False $ 
                      attrs $ do
                        texture $ pigment blue
  return $ 
    scene $ do
      background $ pigment black
      light $
        pointLight (1.0, 4.0, -2.0) white
      object $ do { rotateX 45; rotateY 45 } =!> octahedronMesh
      object checkerFloor

--------------------------------------------------------------------------------

teapot :: IO (Scene (WorldObject Primitive))
teapot = do
  teapotMesh <- mesh "Models/teapot.obj" False $
                  attrs $ do
                    texture $ pigment blue
  return $ 
    scene $ do
      background $ pigment black
      light $
        pointLight (1.0, 4.0, -4.0) white
      object teapotMesh
      object checkerFloor

--------------------------------------------------------------------------------

bunny :: IO (Scene (WorldObject Primitive))
bunny = do
  bunnyMesh <- mesh "Models/bunny.obj" True $
                  attrs $ do
                    texture $ pigment blue
  return $ 
    scene $ do
      background $ pigment black
      light $
        pointLight (1.0, 4.0, -2.0) white
      object bunnyMesh  
      object checkerFloor


venus :: IO (Scene (WorldObject Primitive))
venus = do
  venusMesh <- mesh "Models/venus_small.obj" True $
                  attrs $ do
                    texture $ pigment blue
  return $ 
    scene $ do
      background $ pigment black
      light $
        pointLight (1.0, 4.0, 3.0) white
      object $ do { rotateX 30 } =!> venusMesh
