{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE BangPatterns #-}

module Raytracer.Main where
import System.Environment (getArgs, withArgs)
import Options
import Control.Lens ((^.))
import System.IO
import Text.Printf (printf)
import Data.Array.Repa.IO.BMP (writeImageToBMP)
import Raytracer.Args
import Raytracer.Render (render)
import Raytracer.Scene.Interpreter

--------------------------------------------------------------------------------

defaultArgs :: [String]
defaultArgs = words "-w 120 -h 80 -z 1 -r 1 -s 1 Simple/Simple1"

main :: IO ()
main = do
    args <- getArgs
    withArgs (if null args then defaultArgs else args) run 

run :: IO ()
run = runCommand $ \opts [sceneFile] -> do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    loadResult <- loadScene opts $ fixPath sceneFile
    case loadResult of
      Left errorText -> error errorText
      Right (cam,scene) -> do
        output <- render opts cam scene
        writeImageToBMP (opts^.optOutputFile) output
    where
        fixPath path = printf "Scenes/%s.lua" path