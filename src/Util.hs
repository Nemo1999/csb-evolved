module Util
    ( degToRad
    , normalizeAngle
    , clamp
    , maxTurnAngle
    , boostAccel
    , podForceFieldRadius
    , checkPointRadius
    , podMinCollisionImpact
    , distinctPairs
    , gameWorldSize
    , randomPerm
    , radToDeg
    )
where
import Data.Vec2
import System.Random
import Control.Monad
import Data.Array.IO



-- Parameters ------------------------------------------

gameWorldSize = Vec2 16000 9000

maxTurnAngle :: Double
maxTurnAngle = degToRad 18

boostAccel :: Double
boostAccel = 650

podForceFieldRadius :: Double
podForceFieldRadius = 400

checkPointRadius :: Double
checkPointRadius = 600

podMinCollisionImpact :: Double
podMinCollisionImpact = 120


--Utility Funcitons ------------------------------------

-- | enumerate distinct list  of unordered pair  
distinctPairs :: [a] -> [(a,a)]
distinctPairs xs = concat $ map (\n-> zip (repeat (xs!!n)) (drop (n+1) xs)) [0..(length xs-1)]
{-
-- | randomly permute an array
randomPerm :: [a] -> IO [a]
randomPerm xs 
  | []<-xs =return  []
  | nIO  <- randomRIO (0,length xs - 1) =
      do
        n <- nIO
        rest <- randomPerm (take n xs ++ drop (n+1) xs) 
        return $ (xs!!n):rest
-}
degToRad :: Double -> Double
degToRad = (* (pi / 180))

radToDeg :: Double -> Double
radToDeg = (*(180/pi))

normalizeAngle :: Double -> Double
normalizeAngle x = ((x + pi) `fmod` (2 * pi)) - pi

clamp :: (Ord a) => a -> a -> a -> a
clamp mi x ma = max mi (min x ma)

fmod :: Double -> Double -> Double
fmod x y = x - fromIntegral (floor (x / y))

randomPerm :: [a] -> IO [a]
randomPerm xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
