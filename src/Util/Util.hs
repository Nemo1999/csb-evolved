module Util
    ( degToRad
    , normalizeAngle
    , clamp
    , maxTurnAngle
    , boostAccel
    , podForceFieldRadius
    , checkpointRadius
    , distinctPairs
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

-- | enumerate distinct list  of unordered pair  
distinctPairs :: [a] -> [(a,a)]
distinctPairs xs = concat $ map (\n-> zip (repeat (xs!!n)) (drop (n+1) xs)) [0..(length xs-1)]

degToRad :: Double -> Double
degToRad = (* (pi / 180))

normalizeAngle :: Double -> Double
normalizeAngle x = ((x + pi) `fmod` (2 * pi)) - pi

clamp :: (Ord a) => a -> a -> a -> a
clamp mi x ma = max mi (min x ma)

fmod :: Double -> Double -> Double
fmod x y = x - fromIntegral (floor (x / y))
