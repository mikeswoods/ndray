{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Raytracer.Utils.Data (pick1, pick1Stateful, pickN, randomString)
where
import System.Random (Random, RandomGen, randomR, randomRIO)


lowerAlpha, upperAlpha, digits, allChars :: String
lowerAlpha = ['a' .. 'z']
upperAlpha = ['A' .. 'Z']
digits     = ['0' .. '9']
allChars   = lowerAlpha ++ upperAlpha ++ digits ++ ['_']


pick1 :: [a] -> IO a
pick1 xs = randomRIO (0, length xs - 1) >>= return . (xs !!)


pick1Stateful :: RandomGen g => g -> [a] -> (a, g)
pick1Stateful gen xs = (xs !! i, g')
  where (i, g') = randomR (0, length xs - 1) gen


pickN :: Int -> [a] -> IO [a]
pickN n = sequence . (replicate n) . pick1


randomString :: Int -> IO String
randomString n = pickN n allChars