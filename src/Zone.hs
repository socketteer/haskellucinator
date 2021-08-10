module Zone
    ( zoneZoomAnimation,
      zonePlate,
      pointPropagate,
    ) where


import Graphics.Gloss
    ( makeColor, Color, Point, Display(InWindow), animate )
import Graphics.Gloss.Data.Picture ( Point )
import Graphics.Gloss.Raster.Field
    ( makeColor, Display(InWindow), Color, Point, animateField )


------- Complex patterns -------

pinchZone :: Float -> Float -> Float -> Point -> (Float, Float)
pinchZone t k ω (x, y) = let phase = k * (x^2 - y^2) + ω * t 
                         in (sin phase, cos phase)

zonePlate :: Float -> Float -> Float -> Point -> (Float, Float)
zonePlate t k ω (x, y) = let phase = k * (x^2 + y^2) + ω * t 
                         in (sin phase, cos phase)

-- | Draw the Fresnel transform of a point source with frequency f at distance d
pointPropagate :: Float -> Float -> Point -> (Float, Float)
pointPropagate f d (x, y) = let phase = f * sqrt (x^2 + y^2 + d^2)
                              in (sin phase, cos phase)





-- | Map the real part of a complex number to an RGB color
complexToRGB :: (Float, Float) -> (Float, Float, Float)
complexToRGB (real, imaginary) = let shade = (real + 1) / 2 
                                 in (shade, shade, shade)



------- Color maps -------

-- | Map a complex phase function to a color at time t and point (x, y)
phasePatternZoom :: (Float -> Float -> Float -> Point -> (Float, Float)) -> Float -> Point -> Color
phasePatternZoom f t (x, y) = let (r, g, b) = complexToRGB (f t (20*t*t) 20 (x, y))
                          in makeColor r g b 1.0

-- | Draw a zone plate which changes phase and zooms out with time
zoneZoom :: Float -> Point -> Color
zoneZoom t (x, y) = phasePatternZoom zonePlate t (x, y)

-- | Draw a Fresnel transform changing distance with time
sourceZoom :: Float -> Point -> Color
sourceZoom t (x, y) = let (r, g, b) = complexToRGB (pointPropagate 1000 (t/50) (x, y))
                       in makeColor r g b 1.0





------- IO / Display -------

windowDisplay :: Display
windowDisplay = InWindow "ZonePlate" (700, 700) (100, 100)


zoneZoomAnimation :: IO ()
zoneZoomAnimation = animateField windowDisplay (1, 1) zoneZoom
