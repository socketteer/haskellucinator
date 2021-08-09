module Main where

import Zone

import Graphics.Gloss
    ( Display(InWindow) )

import Graphics.Gloss.Raster.Field

windowDisplay :: Display
windowDisplay = InWindow "ZonePlate" (700, 700) (100, 100)

main :: IO ()
--main = animateField (InWindow "ZonePlate" (700, 700) (100, 100)) (1, 1) (\t (x, y) -> phasePattern pinchZone t (x,y))
main = animateField windowDisplay (1, 1) sourceZoom