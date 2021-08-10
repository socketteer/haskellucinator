module Discrete
    ( 
        rotatingVectorAnimation,
        rotatingVectorSimulation,
        linesTest,
        cornuDisplay,
        cornuAnimation,
        cornuFiniteDisplay,
        cornuFiniteAliasing,
    ) where

import Graphics.Gloss 
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Animate
import Graphics.Gloss.Raster.Field (Display)
import Zone

-- | Create data type for many-dimensional point
-- | and many-dimensional path/collectiob?




------- Vectors / Points -------

-- | Rotate a vector in the x y plane by del
rotateVector :: Float -> Float -> Vector -> Vector
rotateVector t velocity (x, y) = let del = t * velocity 
                                 in (x * cos del - y * sin del, x * sin del + y * cos del) 

-- | Time-parameterized rotating vector in x y plane
rotatingVector :: Float -> Float -> Vector
rotatingVector t velocity = let phase = t * velocity
                            in (100 * cos phase, 100 * sin phase)

updateVector :: ViewPort -> Float -> Vector -> Vector
updateVector viewport t = rotateVector t 5

vectorSum :: Vector -> Vector -> Vector
vectorSum (x, y) (a, b) = (x + a, y + b)


------- Vector arrays -------

-- | Samples a complex function at some interval in some range
sampleFunction :: (Float -> Vector) -> Float -> Float -> Float -> [Vector]
sampleFunction f start end step = map f [start, start + step..end]

sampleZoneplate :: Float -> Float -> Float -> [Vector]
sampleZoneplate = sampleFunction (\x -> zonePlate 0 5 1 (x, 0))

samplePointPropagate :: Float -> Float -> Float -> Float -> Float -> [Vector]
samplePointPropagate f d = sampleFunction (\x -> pointPropagate f d (x, 0))


------- Paths -------

pathIntegral :: [Vector] -> Path
pathIntegral = scanl1 vectorSum

cornuPath :: Float -> Float -> Float -> Path
cornuPath start end step = pathIntegral(sampleZoneplate start end step)

cornuPathFinite :: Float -> Float -> Float -> Float -> Float -> Path
cornuPathFinite f d start end step = pathIntegral(samplePointPropagate f d start end step)



------- Pictures -------


lineFromVector :: Vector -> Picture 
lineFromVector vector = color white(Line[(0,0), vector])

lineFromEndpoints :: (Point, Point) -> Picture
lineFromEndpoints (p1, p2) = color white(Line[p1, p2])

pictureFromPoints :: [(Point, Point)] -> Picture
pictureFromPoints endpoints = Pictures [lineFromEndpoints ep | ep <- endpoints]

cornuPicture :: Float -> Float -> Float -> Picture
cornuPicture start end step = color white(Line(cornuPath start end step))

cornuPictureFinite :: Float -> Float -> Float -> Float -> Float -> Picture
cornuPictureFinite f d start end step = color white(Line(cornuPathFinite f d start end step))



------- IO / Display-------

rotatingVectorAnimation :: IO()
rotatingVectorAnimation = animate simulationWindow black (\t -> lineFromVector (rotatingVector t 10))

rotatingVectorSimulation :: IO()
rotatingVectorSimulation = simulate simulationWindow black 30 (0, 100) lineFromVector updateVector

-- | Display some lines
linesTest :: IO()
linesTest = display simulationWindow black (pictureFromPoints[((0,0), (100,0)), ((0,100), (100,100))])

-- | Display cornu spiral
cornuDisplay :: IO()
cornuDisplay = display simulationWindow black (cornuPicture 0 1000 0.01)

cornuFiniteDisplay :: IO()
cornuFiniteDisplay = display simulationWindow black (cornuPictureFinite 1000 100 0 2000 0.05)

cornuFiniteAliasing :: IO()
cornuFiniteAliasing = animate simulationWindow black (\t -> cornuPictureFinite 1000 100 0 2000 (0.001*t + 0.001))

cornuAliasing :: IO()
cornuAliasing = animate simulationWindow black (\t -> cornuPicture (-1000) 1000 (0.001*t + 0.001))

cornuAnimation :: IO()
cornuAnimation = animate simulationWindow black (\t -> cornuPicture 0 (t*100) 0.005)

simulationWindow :: Display
simulationWindow = InWindow "Simulation" (700, 700) (100, 100)