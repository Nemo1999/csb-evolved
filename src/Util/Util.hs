module Util
    ( degToRad
    , normalizeAngle
    , clamp
    , maxTurnAngle
    , boostAccel
    , podForceFieldRadius
    , checkpointRadius
    )
where

-- Parameters ------------------------------------------

maxTurnAngle :: Double
maxTurnAngle = degToRad 18

boostAccel :: Double
boostAccel = 650

podForceFieldRadius :: Double
podForceFieldRadius = 400

checkpointRadius :: Double
checkpointRadius = 600


--Utility Funcitons ------------------------------------

degToRad :: Double -> Double
degToRad = (* (pi / 180))

normalizeAngle :: Double -> Double
normalizeAngle x = ((x + pi) `fmod` (2 * pi)) - pi

clamp :: (Ord a) => a -> a -> a -> a
clamp mi x ma = max mi (min x ma)

fmod :: Double -> Double -> Double
fmod x y = x - fromIntegral (floor (x / y))
