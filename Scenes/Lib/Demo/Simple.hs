module Scenes.Lib.Demo.Simple
where
import Raytracer.Prelude
import Raytracer.World.Color

--------------------------------------------------------------------------------

s0 :: IO (Scene (WorldObject Primitive))
s0 = do return $ 
          scene $ do
          background $ pigment black
          light $
            pointLight (0.2, 1, 1.0) white
          object $
            plane (0.0, 1.0, 0.0) 1.0 $
              attrs $ do
                diffuse 0.6
                specular 0.7
                ambient 0.3
                --reflect 0.6
                texture $ checkerboard white black 1
          object $
            do { translate 0.0 1.0 0.0 } 
            =!> (sphere 1.0 $
                  attrs $ do
                    ambient 1.0
                    texture $ pigment red)

--------------------------------------------------------------------------------

-- Image mapped sky and plane
s1 :: IO (Scene (WorldObject Primitive))
s1 = do sky    <- imagemap "Sky/001"
        marble <- imagemap "Marble/001"
        return $ 
          scene $ do
            background $ sky
            light $
              pointLight (1, 4, -4.0) white
            object $
              plane (0.0, 1.0, 0.0) 1.0 $
                attrs $ do
                  texture marble
                  ambient 0.4
                  diffuse 0.6

--------------------------------------------------------------------------------

s2 :: IO (Scene (WorldObject Primitive))
s2 = do return $ 
          scene $ do
            background $ ygradient purple red 0.5
            light $
              pointLight (0.5, 3.0, -3.0) white
            object $
              do rotateZ 30
                 rotateX 45
                 translate 0.0 1.0 4.0 
              =!> (sphere 1.0 $
                    attrs $ do
                      texture $ checkerboard red green 0.08
                      --texture $ brick white red 
                      ambient 0.2
                      diffuse 0.7
                      specular 0.9
                      reflect 0.2
                      shine 90)
            -- Rotated cube
            object $
              do rotateX 45 
                 rotateY 45
                 translate (2.0) 1.0 2 
              =!> (box (0.0, 0.0, 0.0) (0.75, 0.75, 0.75) $
                    attrs $ do
                      diffuse 0.9
                      ambient 0.2
                      specular 0.9
                      texture $ pigment blue)
            --object $
            --  plane (0.0, 1.0, 0.0) 1.0 $
            --    attrs $ do
            --      texture $ checkerboard white black 1
            --      ambient 0.4
            --      diffuse 0.6

--------------------------------------------------------------------------------

s3 :: IO (Scene (WorldObject Primitive))
s3 = do return $ 
          scene $ do
            background $ pigment black
            light $
              pointLight (0, 3.0, -2.0) white
            object $
              do { translate 1.0 1.0 2.0 } =!>
                (sphere 1.0 $
                  attrs $ do
                    texture $ pigment green
                    ambient 0.2
                    diffuse 0.3
                    specular 0.9
                    reflect 0.5
                    opacity 0.6
                    refract 3
                    shine 10)
            object $
              plane (0.0, 1.0, 0.0) 1.0 $
                attrs $ do  
                  texture $ checkerboard white black 1
                  ambient 0.4
                  diffuse 0.6

--------------------------------------------------------------------------------

s4 :: IO (Scene (WorldObject Primitive))
s4 = do
  marble001 <- imagemap "Marble/002"
  return $ 
    scene $ do
      background $ ygradient purple red 0.6
      light $
        pointLight (-0.5, 4, -3.0) white
      object $
        do rotateX 45
           rotateY 105
           scaleY 1.0
           translateY 1.5 
        =!>
          (box (-0.5, 0.0, -0.5) (0.5, 1.0, 0.5) $
            attrs $ do
              diffuse 0.3
              ambient 0.3
              specular 0.9
              texture $ marble001)
              --texture $ ygradient yellow blue 0.5)
              --texture $ brick white red)
              --texture $ checkerboard red green 0.25)
              --texture $ pigment blue)
      object $
        plane (0.0, 1.0, 0.0) 0.05 $
          attrs $ do
            diffuse 0.6
            specular 0.7
            ambient 0.3
            --texture $ imagemap "Misc/003"
            texture $ checkerboard white black 2

--------------------------------------------------------------------------------

s5 :: IO (Scene (WorldObject Primitive))
s5 = do 
  jupiter <- imagemap "Planets/earth01"
  return $ 
    scene $ do
      background $ pigment black
      light $
        pointLight (2.0, 4.0, -4.0) white
      object $ 
        sphere 2.0 $
          attrs $ do
            ambient 0.90
            texture jupiter
