{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Options
import Data.Array.Repa.IO.BMP 
  (
   writeImageToBMP
  )
import Control.Lens 
  (
   (^.)
  )
import qualified Raytracer.Render as Render
import qualified Raytracer.Args as Args
import qualified Raytracer.Scenes.Demo as Scenes
import qualified Raytracer.Prelude as P

--------------------------------------------------------------------------------

main :: IO ()
main = runCommand run
  where
    run :: Args.RuntimeOptions -> [String] -> IO ()
    run opts _ = do
      scene <- Scenes.stillLife1
      let camera = P.camera (opts^.Args.optLocation) (opts^.Args.optLookAt) (opts^.Args.optFov) 
      bitmapCanvas <- Render.rendertoBytesArray opts camera scene
      putStrLn $ show opts
      putStrLn $ show camera
      putStrLn $ "> Writing output to " ++ (opts^.Args.optOutputFile)
      writeImageToBMP (opts^.Args.optOutputFile) bitmapCanvas
