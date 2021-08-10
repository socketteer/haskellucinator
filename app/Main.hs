module Main where

import Zone
import Discrete

import Graphics.Gloss
    ( Display(InWindow), animate )


main :: IO ()

--main = rotatingVectorAnimation
--main = zoneZoomAnimation
main = cornuFiniteAliasing