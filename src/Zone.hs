module Zone
    ( sourceZoom,
      phasePattern
    ) where


import Graphics.Gloss
    ( makeColor, Color, Point )

pinchZone :: Float -> Float -> Float -> Point -> (Float, Float)
pinchZone t k ω (x, y) = let phase = k * (x^2 - y^2) + ω * t 
                         in (sin phase, cos phase)

zonePlate :: Float -> Float -> Float -> Point -> (Float, Float)
zonePlate t k ω (x, y) = let phase = k * (x^2 + y^2) + ω * t 
                         in (sin phase, cos phase)

-- | Draw the Fresnel transform of a point source with frequency f at distance d
fresnelZonePlate :: Float -> Float -> Point -> (Float, Float)
fresnelZonePlate f d (x, y) = let phase = f * sqrt (x^2 + y^2 + d^2)
                              in (sin phase, cos phase)

-- | Map the real part of a complex number to an RGB color
complexToRGB :: (Float, Float) -> (Float, Float, Float)
complexToRGB (real, imaginary) = let shade = (real + 1) / 2 
                                 in (shade, shade, shade)

-- | Map a complex phase function like zonePlate or pinchZone to a color at time t and point (x, y)
phasePattern :: (Float -> Float -> Float -> Point -> (Float, Float)) -> Float -> Point -> Color
phasePattern f t (x, y) = let (r, g, b) = complexToRGB (f t (20*t*t) 20 (x, y))
                          in makeColor r g b 1.0

-- | Draw a Fresnel transform, changing distance with time
sourceZoom :: Float -> Point -> Color
sourceZoom t (x, y) = let (r, g, b) = complexToRGB (fresnelZonePlate 400 (t/15) (x, y))
                       in makeColor r g b 1.0