module Warrier.IO where

-- | This module deals with Input Output between process
-- | Use these functions to create Simulator or Player
import Text.Read
import Util
import Data.Vec2
import GameSim
import GameRule
import Player
import Player.GA
import System.IO
import Data.List
import Data.Maybe
import Control.Monad


type Ckpts = [Vec2]

printPoint :: Vec2 -> IO ()
printPoint (Vec2 x y)= do
  putStrLn $ show (round x::Int) ++ show (round y::Int)

readPoint :: IO Vec2
readPoint = do
  [x,y] <- fmap (map read) $ words<$>getLine :: IO [Double]
  return $ Vec2 x y

printCkpts :: GameSpec -> IO ()
printCkpts GameSpec{gameSLaps=laps,gameSCheckpoints=ckpts} = do
  let ckptCount=length ckpts
  putStrLn $ show laps
  putStrLn $ show ckptCount
  forM_ ckpts printPoint 
  
readCkpts :: IO (Int , Ckpts)
readCkpts = do
  laps <- read<$>getLine :: IO Int
  ckptCount <- read<$>getLine :: IO Int 
  ckpts <- sequence $ replicate ckptCount readPoint
  return (laps,ckpts)

printPod :: GameSpec -> PodState -> IO ()
printPod GameSpec{gameSCheckpoints=ckpts}PodState{podPosition=(Vec2 x y),podSpeed=(Vec2 vx vy),
                                                  podAngle=(Just ang),
                                                  podNextCheckPoints=podCkpts} = do
  let ckptId = fromJust $ elemIndex (head podCkpts) ckpts
  let [xI,yI,vxI,vyI] = map round [x,y,vx,vy] :: [Int]
  let angleDeg =round $ radToDeg ang :: Int
  putStrLn $ foldl (\str i-> str++show i++" ") "" [xI,yI,vxI,vyI,angleDeg,ckptId]

readPod   :: (Int , Ckpts)-> IO PodState
readPod  (laps, ckpts)= do
  [x,y,vx,vy,angle,ckptId] <- fmap (map read) $  words<$>getLine :: IO [ Int ]
  let pos = Vec2 (fromIntegral x) (fromIntegral y)
  let speed = Vec2 (fromIntegral vx) ( fromIntegral vy)
  let ang = Nothing -- Just $ fromIntegral angle ?
  let podCkpts = take (laps*length ckpts) (tail $ cycle ckpts)
  return emptyPodState{podPosition=pos,podSpeed=speed,podAngle=ang,podNextCheckPoints=podCkpts}

updatePod :: Ckpts -> PodState -> IO PodState
updatePod ckpts podPrev = do
  [x,y,vx,vy,degree,ckptId] <- fmap (map read) $  words<$>getLine :: IO [ Int ]
  let pos = Vec2 (fromIntegral x) (fromIntegral y)
  let speed = Vec2 (fromIntegral vx) ( fromIntegral vy)
  let ang = Just $ degToRad $fromIntegral degree
  let podCkpts = dropWhile (/= (ckpts!!ckptId)) (podNextCheckPoints podPrev)
  return podPrev{podPosition=pos ,podSpeed = speed , podAngle = ang,podNextCheckPoints=podCkpts}
        
updateShieldThrust :: PodState -> PodState
updateShieldThrust ps@PodState{podBoostAvail=ba,podShieldState=sh,podMovement=(PodMovement _ boost)} =
  let sh' = shieldNextState (boost==Shield) sh
      ba' = if ba then (boost /= Boost) else ba
  in  ps{podBoostAvail = ba',podShieldState = sh'}

readMovement :: IO PodMovement
readMovement = do
  [xS,yS,thrustS]<-words<$>getLine
  let x = read xS :: Double
  let y = read yS :: Double
  let thrust = case thrustS of
        "BOOST" -> Boost
        "SHIELD" -> Shield
        otherwise -> case readMaybe thrustS of
          Just n -> Normal n
          Nothing -> error ("illegal thrust value: "++thrustS)
  return $ PodMovement (Vec2 x y) thrust
  
putMovement :: PodMovement -> IO ()
putMovement (PodMovement (Vec2 x y) thrust) =
  let xI = round x :: Int
      yI = round y :: Int
      thrustStr = case thrust of
        Boost -> "BOOST"
        Shield -> "SHIELD"
        Normal n -> show n
  in  putStrLn (show xI ++ " " ++ show yI ++ " " ++ thrustStr) 

logStr :: String -> IO ()
logStr = hPutStrLn stderr

