module Raytracer.Scenes.Demo
where
import Raytracer.Prelude
import Raytracer.World.Color

--------------------------------------------------------------------------------

--  -S "stillLife1" -w 80 -h 80 -z 1 -r 1 -s 1  -l "2.0,2,-6.0" -a "0.0,1.0,0.0"
stillLife1 :: IO Scene
stillLife1 =
  return $ scene $ do
    environment $ ygradient purple red 0.5
    light $
      pointLight (1.0, 3.0, -4.0) white
    -- Checkerboard sphere
    object $
      do rotateZ 30
         rotateX 45
         translate 0 1 4
      =!> (sphere 0 1.0 $
            attrs $ do 
              textureMap $ do 
                mapToSphere (checkerboard red green 0.1) usingPosition
              ambient 0.1
              diffuse 0.7
              specular 0.9
              reflect 0.2
              shine 30)
    -- Glass sphere
    object $
      do translate (-2.0) 0.5 1
      =!> (sphere 1 0.75 $
            attrs $ do
              textureMap $ do
                mapToSphere (pigment white) usingPosition
              ambient 0.3
              diffuse 0.7
              specular 0.9
              reflect 0.4
              opacity 0.2
              refract 4
              shine 20)
    -- Rotated cube
    object $
      do rotateX 25 
         rotateY 45
         translate 0.5 0 0 
      =!> (box 2 (0.0, 0.0, 0.0) (0.75, 0.75, 0.75) $
            attrs $ do
              textureMap $ do
                mapToSphere (pigment blue) usingPosition
              diffuse 0.9
              ambient 0.2
              specular 0.9)
    object $
      plane 3 (0.0, 1.0, 0.0) 1.0 $
        attrs $ do
          textureMap $ do
            mapToY (checkerboard white black 1) usingPosition
          ambient 0.4
          diffuse 0.6

--------------------------------------------------------------------------------

--  -S "stillLife2" -w 80 -h 80 -z 1 -r 1 -s 1  -l "2.0,2,-6.0" -a "0.0,1.0,0.0"
--stillLife2 :: IO Scene
--stillLife2 = do
--  sky     <- imagemap "Sky/002"
--  marble1 <- imagemap "Marble/001"
--  marble2 <- imagemap "Marble/005"
--  marble3 <- imagemap "Marble/010"
--  jupiter <- imagemap "Planets/jupiter01"
--  icosahedronMesh <- mesh "Models/icosahedron.obj" False $
--                       attrs $ do
--                         texture $ pigment green
--                         diffuse 0.9
--                         specular 0.9
--                         ambient 0.05
--                         opacity 0.8
--  octahedron <- mesh "Models/octahedron.obj" False $
--                  attrs $ do
--                    texture $ pigment red
--                    diffuse 0.9
--                    specular 0.9
--                    ambient 0.05
--                    opacity 0.5
--  return $ scene $ do
--    environment $ 
--      sky
--      --ygradient teal white 0.5
--    light $
--      pointLight (0.5, 2.0, -5.0) white
--    object $ 
--      plane (0.0, 1.0, 0.0) 1.0 $
--        attrs $ do
--          texture $ checkerboard white black 0.5
--          ambient 0.3
--          diffuse 1.0
--          specular 0.0
--          reflect 0.1
--    -- Box #1
--    object $
--      do  translate (-1.75) (-1) (-0.25)
--        =!> (box (0,0,0) (0.5,1.0,0.5) $
--               attrs $ do
--                 texture marble1
--                 diffuse 0.1
--                 specular 0.1
--                 ambient 0.3)
--    -- Box #1 diamond
--    object $
--      do  translate (-1.5) 0.33 0
--          scale 0.33 0.33 0.33
--        =!> octahedron

--    -- Box #2
--    object $
--      do  translate (-0.25) (-1) (-0.25)
--        =!> (box (0,0,0) (0.5,1.0,0.5) $
--               attrs $ do
--                 texture $ marble2
--                 diffuse 0.2
--                 specular 0.2
--                 ambient 0.3)
--    -- Box #2 sphere
--    object $
--      do  scale 0.33 0.33 0.33
--          translate 0.0 0.4 0
--        =!> (sphere 1.0 $
--               attrs $ do
--                 texture $ pigment blue
--                 ambient 0.2
--                 diffuse 0.3
--                 specular 0.8
--                 reflect 0.5
--                 shine 50)

--    -- Box #3
--    object $
--      do  translate (1.25) (-1) (-0.25)
--        =!> (box (0,0,0) (0.5,1.0,0.5) $
--               attrs $ do
--                 texture $ marble3
--                 diffuse 0.2
--                 specular 0.2
--                 ambient 0.3)
--    -- Box #3 icosahedron
--    object $
--      do  translate (1.5) 0.33 0
--          scale 0.33 0.33 0.33
--        =!> icosahedronMesh

--    object $
--      do translate (-2) (-0.7) (-1)
--         =!> (sphere 0.3 $
--                attrs $ do
--                  texture jupiter
--                  ambient 0.2
--                  diffuse 0.6)

--------------------------------------------------------------------------------

-- Outer space scene w/ teapot
-- ./rt -S "space1" -w 800 -h 600 -z 1 -r 1 -s 1  -l "-2.0,2,-9.0" -a "0.0,1.0,0.0" +RTS -N2 -s
--space1 :: IO Scene
--space1 = do 
--  stars <- imagemap "Planets/stars01"
--  earth <- imagemap "Planets/earth01"
--  moon  <- imagemap "Planets/moon01"
--  teapotMesh <- mesh "Models/teapot.obj" False $
--                  attrs $ do
--                    texture $ pigment gray
--                    diffuse 0.9
--                    specular 0.9
--                    ambient 0.1
--                    shine 50
--                    reflect 0.3
--  return $ 
--    scene $ do
--      environment stars
--      light $
--        pointLight (-4.0, 3.0, -5.0) white
--      object $ do rotateY 30
--                  rotateZ 15 
--                  translateX $ -1
--                  translateY 1 
--        =!> (sphere 2.0 $
--               attrs $ do
--                 ambient 0.90
--                 texture earth)
--      object $ do translateX $ 1.5
--                  translateY 2.75 
--        =!> (sphere 0.2 $
--               attrs $ do
--                 ambient 0.90
--                 texture moon)
--      object $ do scale 0.33 0.33 0.33
--                  rotateX 30 
--                  rotateY $ -20
--                  translateX 0.25
--                  translateZ $ -2.5
--        =!> teapotMesh
