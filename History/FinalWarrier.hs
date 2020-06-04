{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns#-}
{-# OPTIONS_GHC -O2  #-}
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns#-}
{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -threaded #-}
{-# OPTIONS_GHC -O2  #-}
import Data.Array.IO
import Data.Maybe
import Data.Function
import Data.List
import System.Random
import System.IO
import Control.Monad
import System.Timeout
import System.CPUTime
import Debug.Trace
import System.Random
import Text.Read
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import System.Random
import Control.Monad.Fix
import Data.Maybe
import Data.List
import qualified Data.Vector as V  
import           Debug.Trace
import           Data.List
import           Data.Maybe
import Data.Ratio
import Data.List
import Data.Maybe
import Control.Parallel
import Control.Parallel.Strategies hiding (dot)
import System.Random
import System.Timeout
import Data.Time.Clock
import Debug.Trace
import Data.List
import System.Random
import Control.Monad
import Data.Array.IO

{-
-- import GameRule
-}
--import Control.Parallel

----------------Warrier Start------------------------
gameCycles :: (PlayerIO p) => Ckpts -> [PodState]-> p -> IO ()
gameCycles ckpts [p1,p2,o1,o2] player = do
   [p1',p2',o1',o2'] <- sequence $ map (updatePod ckpts) [p1,p2,o1,o2]
   
   let playerIn = PlayerIn [p1',p2'] [o1',o2']
   
   ([move1,move2] , player') <- playerRunIO player playerIn
   
   let (p1'' ,p2'')= (p1'{podMovement = move1} ,p2'{podMovement=move2})
   
   let [p1''',p2'''] = map updateShieldThrust [p1'',p2'']
   
   putMovement move1
   putMovement move2
   gameCycles ckpts [p1''',p2''',o1',o2'] player'



   
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    
    cInfo@(laps,ckpts) <- readCkpts
    pod1  <- readPod cInfo
    pod2  <- readPod cInfo
    opp1  <- readPod cInfo
    opp2  <- readPod cInfo
    
    player <- playerInitIO $ defaultGAGuess --WrapIO $  ElementaryPlayer ()
    let playerIn = PlayerIn [pod1,pod2] [opp1,opp2]
    ([move1,move2] , player' )  <- playerRunIO player playerIn
    putMovement move1
    putMovement move2
    let (pod1' ,pod2')= (pod1{podMovement = move1} ,pod2{podMovement=move2})
    let [podStart1,podStart2] = map updateShieldThrust [pod1',pod2']
    -- game loop
    gameCycles ckpts [podStart1,podStart2,opp1,opp2] player'



data Vec2 = Vec2 {-# UNPACK #-} !(Double)
                 {-# UNPACK #-} !(Double)  deriving (Show, Read, Eq)

instance Num Vec2 where
    (+)    = elementWise2 (+)
    (-)    = elementWise2 (-)
    (*)    = elementWise2 (*)
    negate = elementWise negate
    abs    = elementWise abs
    signum = elementWise signum
    fromInteger x = Vec2 (fromInteger x) (fromInteger x)

elementWise :: (Double -> Double) -> Vec2 -> Vec2
elementWise f (Vec2 x y) = Vec2 (f x) (f y)

elementWise2 :: (Double -> Double -> Double) -> Vec2 -> Vec2 -> Vec2
elementWise2 f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 `f` x2) (y1 `f` y2)

scalarMul :: Double -> Vec2 -> Vec2
scalarMul !c (Vec2 !x !y) = Vec2 (c * x) (c * y)

scalarDiv :: Vec2 -> Double -> Vec2
scalarDiv (Vec2 !x !y) !c = Vec2 (x / c) (y / c)

dot :: Vec2 -> Vec2 -> Double
(Vec2 !x1 !y1) `dot` (Vec2 !x2 !y2) = x1 * x2 + y1 * y2

norm :: Vec2 -> Double
norm !v = sqrt $ dot v v

dist :: Vec2 -> Vec2 -> Double
dist v1@(Vec2 !x1 !y1) v2@(Vec2 !x2 !y2) = norm $ v1 - v2

arg :: Vec2 -> Double
arg (Vec2 !x !y) = atan2 y x

-- | project v1 onto v2
proj :: Vec2 -> Vec2 -> Vec2
proj v1@(Vec2 !x1 !y1) v2@(Vec2 !x2 !y2) = ((v1 `dot` v2)/(v2 `dot` v2)) `scalarMul` v2

angleBetween :: Vec2 -> Vec2 -> Double
angleBetween from to = signum (rotate90 from `dot` to)
    * acos ((from `dot` to) / (norm from * norm to))

rotate :: Double -> Vec2 -> Vec2
rotate theta (Vec2 !x !y) =
    Vec2 (cos theta * x - sin theta * y) (sin theta * x + cos theta * y)

unitVec :: Double -> Vec2
unitVec theta  = Vec2 (cos theta) (sin theta) 

rotate90 :: Vec2 -> Vec2
rotate90 (Vec2 !x !y) = Vec2 (-y) x

reflect :: Vec2 -> Vec2 -> Vec2
reflect mirror !v = (2 `scalarMul` projection) - v
  where
    mirrorUnit = mirror `scalarDiv` norm mirror
    projection = (mirrorUnit `dot` v) `scalarMul` mirrorUnit

isZero :: Vec2 -> Bool
isZero (Vec2 !x !y) = x == 0 && y == 0

-- |zero Vector
zeroVec = Vec2 0 0

-- | random point in the rectangle area defined by p1 and p2
randomVec :: Vec2 -> Vec2 -> IO Vec2
randomVec p1@(Vec2 !x1 !y1) p2@(Vec2 !x2 !y2)
 = let (minX,maxX)=(min x1 x2 , max x1 x2)
       (minY,maxY)=(min y1 y2 , max y1 y2)
   in  Vec2 <$> (randomRIO (minX,maxX)) <*> (randomRIO (minY,maxY))

roundVec  :: Vec2 -> Vec2
roundVec (Vec2 !x !y) = Vec2 (fromIntegral $ round x) (fromIntegral $ round y)  
  
normalize :: Vec2 -> Vec2
normalize v = v `scalarDiv` norm v

-- | This module deals with Input Output between process
-- | Use these functions to create Simulator or Player


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



maxSimTurn = 1000 :: Int

-- | Informaiton about the GameSetting
data GameSpec = GameSpec { gameSLaps :: Int
                         , gameSCheckpoints :: [Vec2] } deriving (Show, Read)


-- Game history , the time goes from right to left in the list 
type GameHistory = [[PodState]]



emptyMovement = PodMovement zeroVec $ Normal 0
emptyPodState = PodState zeroVec zeroVec Nothing True Nothing emptyMovement []


initPodStates :: GameSpec -> IO([PodState])
initPodStates (GameSpec laps ckpts) =
  let podckpts =take (length ckpts * laps) $ tail $cycle ckpts
      (ckpt0,ckpt1) = (head ckpts , head $ tail ckpts)
      perp = rotate90 (ckpt1-ckpt0)
      shift = (250 / norm perp) `scalarMul` perp
      podPos = [ckpt0+shift,
                ckpt0 + 3`scalarMul`shift,
                ckpt0-shift,
                ckpt0 - 3`scalarMul`shift]
  --in randomPerm $  map (\pos->emptyPodState{podPosition=pos,podNextCheckPoints=podckpts}) podPos
  in   return $ map (\pos->emptyPodState{podPosition=pos,podNextCheckPoints=podckpts}) podPos



-- | generate random gamespec
randomGameSpecIO :: IO GameSpec
randomGameSpecIO = do
    nCkpts <- randomRIO (3, 8)::IO Int
    ckpts  <- genCkpts nCkpts
    return $ GameSpec 3 ckpts
  where
    genCkpts 0 = return []
    genCkpts n = do
        ckpts <- genCkpts (n - 1)
        ckpt  <- fix $ \rec -> do
            ckpt <- randomVec zeroVec gameWorldSize

            if hasOverlap ckpt ckpts then rec else return ckpt

        return $ ckpt : ckpts

    hasOverlap ckpt =
        any (\ckpt' -> dist ckpt ckpt' < 2 * checkPointRadius) 


gameEnd :: GameHistory -> Bool
gameEnd = any (\p->podNextCheckPoints p == []) . head  


playerDrivePod :: (PlayerIO p1, PlayerIO p2) => p1 -> p2 -> [PodState] -> IO (p1 , p2 ,[PodState]) 
playerDrivePod p1 p2 g = do                                                              
    let p1In = PlayerIn (take 2 g) (drop 2 g)                                            
    let p2In = PlayerIn (drop 2 g) (take 2 g)                                            
    ( [p1Out1,p1Out2] , p1') <- playerRunIO p1 p1In                                       
    ( [p2Out1,p2Out2] , p2') <- playerRunIO p2 p2In                                       
    let g' = [(g!!0){podMovement=p1Out1},(g!!1){podMovement=p1Out2},                     
          (g!!2){podMovement=p2Out1},(g!!3){podMovement=p2Out2}]                         
    return (p1', p2',g')                                                                 



-- | initialize and simulate the Game
runGame ::
  forall player1  player2. (PlayerIO player1 , PlayerIO player2) =>
  (player1 , player2)
  -> GameSpec 
  -> (GameHistory -> Bool)  -- Stop Rule 
  -> IO GameHistory        
  
runGame (p1,p2) gameSpec stopRule = do
    p1Init <-playerInitIO p1 :: IO player1
    p2Init <-playerInitIO p2 :: IO player2
    g0  <-  initPodStates gameSpec
    
    (p1',p2',g1) <- playerDrivePod p1Init p2Init g0
    let g1' = map (thrustPod.rotatePod) g1
    
    simulate p1' p2' [g1'] stopRule 


-- | simulate the game
simulate ::
  forall player1 player2. (PlayerIO player1 , PlayerIO player2) =>
  player1
  -> player2
  -> [[PodState]] -- Initial State
  -> (GameHistory -> Bool)  -- stop rule 
  -> IO GameHistory

simulate p1 p2 gss@(g:gs) stopRule = 
  if (length gss > maxSimTurn) || stopRule gss
    then playerFinalizeIO p1 >> playerFinalizeIO p2 >> return gss else do
    let g0 = map speedDecay $ movePods 1 g
    --seq g0 $ putStrLn "moved"
    (p1',p2',g1) <- playerDrivePod p1 p2 g0
    --putStrLn "Drived"
    let g2 =  map (roundTrunc.thrustPod.rotatePod) g1
    --seq g2 $ putStrLn "thrusted"
    simulate p1' p2' (g2:gss) stopRule
      
      


type Angle = Double

data PodState = PodState { podPosition          :: !Vec2
                         , podSpeed             :: !Vec2
                         , podAngle             :: !(Maybe Angle)
                         , podBoostAvail        :: !Bool
                         , podShieldState       :: !ShieldState
                         , podMovement          :: !PodMovement
                         , podNextCheckPoints   :: ![Vec2]
                         } deriving (Eq,Show,Read)


data PodMovement = PodMovement { podTarget :: Vec2
                               , podThrust :: Thrust
                               } deriving (Show,Read, Eq)


data Thrust = Normal Int | Shield | Boost deriving (Show,Read,Eq)



type ShieldState = Maybe Int 
shieldNextState :: Bool -> ShieldState -> ShieldState
shieldNextState !activated !ss = if activated then Just 3 else
                                          case ss of
                                            Just n -> if n>0 then Just (n-1) else Nothing
                                            Nothing -> Nothing




-- | simulate Game Phisic for n turn
gameSimTurns ::Int -> [PodState] -> [PodState]
gameSimTurns 0 !pss = pss
gameSimTurns n !pss = gameSimTurns (n-1) $ map (roundTrunc.speedDecay) $ movePods 1 $ map (thrustPod.rotatePod) pss
{-
-- | simulate Game Physic for less than 1 turn
gameSimTime :: Time -> [PodState] -> [PodState]
gameSimTime t pss = movePods t $ map (thrustPod.rotatePod) pss 
-}

-- | Rotate Pods (change angle)
rotatePod :: PodState -> PodState
rotatePod ps@(PodState position _ ang _ _ (PodMovement target thrust) _ )
  -- at fist turn , turn to the target  
  | Nothing    <- ang = ps{podAngle = Just $  arg (target - position)}
  | Just angle <- ang =
      let deltaAngle = normalize (arg (target - position) - angle)
          normalize !th
            | th > pi  = th - 2*pi
            | th <= (-pi) = th + 2*pi
            | otherwise = th
          r = maxTurnAngle -- range of turning angle
          angle' = normalize $ (angle + (clamp (-r) deltaAngle r))
      in  ps{podAngle = Just angle'}
         

-- | Update the PodState ( speed , boost , shield ) according to current PodMovement 
thrustPod :: PodState -> PodState 
thrustPod ps@(PodState position speed (Just angle) boostAvail shieldState (PodMovement target thrust) _ )=
     let
         shieldState' = shieldNextState (thrust == Shield) shieldState
         idle = isJust shieldState'         
         accMag = if idle then 0 else
           case thrust of
             Normal n -> fromIntegral $ clamp 0 n 200 
             Boost    -> if boostAvail then boostAccel else 200
         acc  = accMag  `scalarMul` (unitVec angle)
         speed'= if idle then speed else speed + acc
         boostAvail' = boostAvail && (thrust /= Boost)
      in
         ps{podSpeed=speed',podBoostAvail=boostAvail',podShieldState = shieldState'}

-- | list index of a pod
type PodID = Int
-- | represent time in a single term  (between 0 and 1)
type Time  = Double

-- | Move Pod according to PodState (Including Collision)
movePods :: Time ->  [PodState] -> [PodState]
movePods duration pss = movePods' 0 pss 
  where
        movePods' :: Time -> [PodState] -> [PodState]
        movePods' currentT pss
          -- No collision happen
          | isNothing $ firstCollision pss = map (driftPod (duration-currentT)) pss
          -- First collision happens after desired duration
          | Just(_,collideT)<-firstCollision pss,(collideT+currentT)>duration = map (driftPod (duration-currentT)) pss        -- Collision happens
          | Just((p1,p2),collideT)<-firstCollision pss =
              let (p1',p2')= (driftPod collideT p1,driftPod collideT p2)
                  (p1'',p2'') = collide2Points (p1',p2')
                  result1 =  map (driftPod collideT) pss
                  result2 = map (\p'-> if p'==p1' then p1'' else if p'== p2' then p2'' else p') result1
              in  movePods' (collideT+currentT) result2
              
-- | drift a pod for time 'dt' with its speed , update checkpoints if it pass one             
driftPod ::  Time -> PodState ->  PodState                                                                   
driftPod (!dt) pod = 
  let 
      ckpts = podNextCheckPoints pod
      ckpt = head ckpts
      position  = podPosition pod
      reletivePos  = position - ckpt
      speed = podSpeed pod
      position' =  position + (dt `scalarMul` speed)
      radius = checkPointRadius-30
      reachTime = collideTime reletivePos speed radius
      ckpts'
        | ckpts == []  = []
        | Just t <-reachTime , t  < dt =  tail ckpts
        | otherwise = ckpts
  in
      pod{podPosition =position',podNextCheckPoints = ckpts'}
      


-- | Find the first upcoming collision time and pair of Pods
firstCollision :: [PodState]  -> Maybe ( (PodState,PodState) , Time )  
firstCollision pss  =
  let folder prev new
        | Nothing  <- prev    =  new
        | Nothing  <- new     =  prev
        | Just (_,t') <- new , Just (_,t) <-prev = if t' < t then new else prev
  in
     foldl' folder Nothing  [ collideDetect (p1,p2) | (p1,p2) <- distinctPairs pss ]

  
-- | Detect the time before collision of 2 Pod
collideDetect :: (PodState,PodState)-> Maybe ( (PodState , PodState) , Time )
collideDetect pair@(pod1,pod2) = 
  let (p1,p2) = (podPosition (pod1),podPosition (pod2))
      (v1,v2) = (podSpeed    (pod1),podSpeed    (pod2))
      -- fix the origin of the coordinate to p1
      p2' = p2 - p1
      v2' = v2 - v1
      radius = podForceFieldRadius * 2
      boomTime = collideTime p2' v2' radius
  in
      fmap (\t->(pair,t)) boomTime   

-- | Find the collision time with  origin
collideTime :: Vec2 -> Vec2 -> Double -> Maybe Time
collideTime position speed  radius =
  let nearest = position - (position `proj` speed)
      minDistSquare = nearest `dot` nearest
      radiusSquare =  radius * radius
      -- below work only if minDist < radius , otherwise "sqrt" return error
      distBeforeCollide = (norm (position `proj` speed)) - sqrt (radiusSquare - minDistSquare)
      timeBeforeCollide = distBeforeCollide / norm speed
  in
      case () of
        _
          -- the point is moving further away
          | speed `dot` position >= 0 -> Nothing
          -- nearest point is not close enough
          | minDistSquare > radiusSquare -> Nothing
          -- collision will happen 
          | otherwise -> Just timeBeforeCollide


-- | given two pods p1 p2 touching eachother , update their speed after collision
collide2Points :: (PodState , PodState) -> (PodState,PodState)
collide2Points (pod1, pod2)  =
  let 
      (p1,p2,v1,v2) = (podPosition pod1 , podPosition pod2 , podSpeed pod1 , podSpeed pod2)
      -- move the origin of coordinate to p1
      (p1',v1') = (Vec2 0 0 , Vec2 0 0)
      (p2',v2') = (p2 - p1 , v2 - v1 )
      --the mass of the pod
      m1 = if (podShieldState  pod1) == Just 3 then 10 else 1
      m2 = if (podShieldState  pod2) == Just 3 then 10 else 1
      -- the impact is the
      impactCoefficiant = 2*((m1*m2)/(m1+m2))
      impact = impactCoefficiant `scalarMul` (v2' `proj`  p2')
      -- if the impact vector is shorter than minima , normalize it  
      impact' = if norm impact < podMinCollisionImpact
        then ( podMinCollisionImpact /norm impact) `scalarMul` impact
        else impact
      pod1' = pod1{podSpeed = v1 + (impact `scalarDiv` m1)  }
      pod2' = pod2{podSpeed = v2 - (impact `scalarDiv` m2) }
  in  
      (pod1',pod2')
-- | Decay speed of a pod due to friction   
speedDecay :: PodState -> PodState
speedDecay ps@PodState{podSpeed = speed} = ps{podSpeed = 0.85 `scalarMul` speed}

-- | Round the position and truncate the speed
roundTrunc :: PodState -> PodState
roundTrunc ps@PodState { podPosition = r, podSpeed = v } = ps
  { podPosition = r'
  , podSpeed    = v'
  }
 where
  r' = elementWise (fromIntegral . round) r
  v' = elementWise (fromIntegral . truncate) v

-----------TESTING UTILITY-------------
{-
zeroV = Vec2 0 0 

pos :: PodState -> Vec2
pos p = podPosition p

zeroPod = PodState zeroV zeroV Nothing True Nothing (PodMovement zeroV (Normal 0)) []

drifter :: Vec2 -> Vec2 -> PodState
drifter pos speed = zeroPod{podPosition = pos,podSpeed = speed}

mover  :: Vec2 -> Vec2  -> PodState
mover   pos speed = let ans = (drifter pos speed){podMovement = PodMovement (podPosition ans + podSpeed ans) (Normal 100)}
  in ans 

attracter :: Vec2 -> Vec2 -> PodState
attracter pos target = (drifter pos V.zeroVec){podMovement = PodMovement target  (Normal 100)}



roundn :: [PodState]->Int->[PodState]
roundn init n =  (!!n) $ iterate gameSimTurn init

rounds :: [PodState]->Int-> IO()
rounds init n = mapM_ (\x->putStrLn $show x) $ take n $ (map (map podAngle) $iterate gameSimTurn init)


pod1 = drifter (Vec2 1000 0) (Vec2 100 0)
pod2 = drifter (Vec2 2000 0) (Vec2 (-100) 0)

game = [attracter V.zeroVec (Vec2 500 0)]

roundt :: [Double] -> IO()
roundt = mapM_ (\t -> putStrLn $ show $ map podAngle $  gameSimTime t game)
-}



data PlayerIn = PlayerIn {selfPod ::[PodState]
                         ,oppoPod ::[PodState]} 

type PlayerOut = [PodMovement]

class Player p where
    playerInit :: p -> p  
    playerRun  :: p -> PlayerIn -> (PlayerOut, p)
    
class PlayerIO p where
    playerInitIO :: p -> IO p
    playerRunIO  :: p -> PlayerIn -> IO (PlayerOut , p)
    playerFinalizeIO :: p -> IO ()
    playerFinalizeIO _ = return ()


newtype WrapIO p = WrapIO p

-- | every Player p can be used as PlayerIO p     
instance (Player p) => PlayerIO (WrapIO p) where  
  playerInitIO (WrapIO !p)  = return  $ WrapIO $ playerInit p
  playerRunIO  (WrapIO !p)  !pin =
    let (!pout, !p') = playerRun p pin  in return (pout, WrapIO p')

-- Helper funtion
wrapAngle :: (Vec2,Angle) -> Thrust -> PodMovement 
wrapAngle (pos , ang ) thrust =
  PodMovement (pos + roundVec (20 `scalarMul` unitVec ang )) thrust

    
-- Testing ------------------


instance Player Int  where
  playerInit = id
  playerRun p  _  = ([],p+1)

instance PlayerIO Bool where
  playerInitIO = return 
  playerRunIO  p _  = return $ ([],not p )


-- newtype IdlePlayer = IdlePlayer () deriving (Show)



newtype DefaultPlayer = DefaultPlayer () deriving (Show)

instance Player DefaultPlayer where
    playerInit = id
    playerRun _ _ = (replicate 2 movementPerPod, DefaultPlayer ())
      where
        movementPerPod =
            PodMovement { podTarget = Vec2 8000 4500, podThrust = Normal 100 }

newtype ElementaryPlayer = ElementaryPlayer () deriving (Show)

instance Player ElementaryPlayer where
    playerInit = id
    playerRun _ PlayerIn { selfPod = ss }  =
        (map straightToTarget ss, ElementaryPlayer ())
      where
        straightToTarget PodState { podNextCheckPoints = ckpts } = PodMovement
            { podTarget = case ckpts of
                              []         -> Vec2 0 0
                              target : _ -> target
            , podThrust = Normal 100
            }

newtype ElementarySearchPlayer = ElementarySearchPlayer () deriving (Show)

instance Player ElementarySearchPlayer where
  playerInit = id

  playerRun _ PlayerIn { selfPod = ss } =
    (map decision ss, ElementarySearchPlayer ())
   where
    nRounds     = 6
    nCandidates = 100

    decision :: PodState -> PodMovement
    decision podState =
      espPodMovementToPodMovement podState $ searchOptimalMovement podState

    searchOptimalMovement :: PodState -> ESPPodMovement
    searchOptimalMovement podState = search nRounds [initSearchState]
     where
      search 0 (SearchState { searchStateFirstMovement = Just m } : _) = m
      search n searchStates = search (pred n) searchStates'
       where
        searchStates' =
          take nCandidates
            . sortOn searchStateMetric
            . concatMap deriveSearchState
            $ searchStates

      initSearchState = SearchState { searchStatePodState           = podState
                                    , searchStateFirstMovement      = Nothing
                                    , searchStateNChecrkPointPassed = 0
                                    }

      movements =
        [ ESPPodMovement { podMovementTurnAngle = (-pi) / 10
                         , podMovementThrust    = Normal 0
                         }
        , ESPPodMovement { podMovementTurnAngle = 0
                         , podMovementThrust    = Normal 0
                         }
        , ESPPodMovement { podMovementTurnAngle = pi / 10
                         , podMovementThrust    = Normal 0
                         }
        , ESPPodMovement { podMovementTurnAngle = (-pi) / 10
                         , podMovementThrust    = Normal 100
                         }
        , ESPPodMovement { podMovementTurnAngle = 0
                         , podMovementThrust    = Normal 100
                         }
        , ESPPodMovement { podMovementTurnAngle = pi / 10
                         , podMovementThrust    = Normal 100
                         }
        ]
      passedCheckPoint PodState { podNextCheckPoints = (p : _) } PodState { podNextCheckPoints = (q : _) }
        = p /= q
      deriveSearchState SearchState { searchStatePodState = podState, searchStateFirstMovement = m, searchStateNChecrkPointPassed = n }
        = map f movements
       where
        f movement =
          let podState' = simulate podState movement
          in
            SearchState
              { searchStatePodState           = podState'
              , searchStateFirstMovement      = initialize movement m
              , searchStateNChecrkPointPassed =
                if passedCheckPoint podState podState' then succ n else n
              }

      searchStateMetric SearchState { searchStatePodState = PodState { podPosition = r, podNextCheckPoints = (p : _) }, searchStateNChecrkPointPassed = n }
        = (-n, dist r p)

    -- Simulation ---------------------------------------------------------

    espPodMovementToPodMovement :: PodState -> ESPPodMovement -> PodMovement
    espPodMovementToPodMovement PodState { podPosition = r, podAngle = theta } ESPPodMovement { podMovementTurnAngle = deltatheta, podMovementThrust = thr }
      = PodMovement { podTarget = target, podThrust = thr }
     where
      target =
        1000 `scalarMul` rotate (fromMaybe 0 theta + deltatheta) (Vec2 1 0) + r

    simulate :: PodState -> ESPPodMovement -> PodState
    simulate podState ESPPodMovement { podMovementTurnAngle = deltatheta, podMovementThrust = thr }
      = podState { podPosition        = r'
                 , podSpeed           = v'
                 , podAngle           = Just theta'
                 , podNextCheckPoints = ps'
                 }
     where
      PodState { podPosition = r, podSpeed = v, podAngle = theta, podNextCheckPoints = (p : ps) }
        = podState

      -- Rotation.
      theta' = normalizeRad $ fromMaybe 0 theta + deltatheta

      -- Acceleration.
      am     = case thr of
        Normal x -> fromIntegral x
        Boost    -> 650
        Shield   -> 0
      a            = am `scalarMul` rotate theta' (Vec2 1 0)
      v''          = v + a

      -- Movement.
      r'           = r + v''

      -- Friction.
      v'           = 0.85 `scalarMul` v''

      -- Checkpoint.
      inCheckpoint = dist r' p < 600
      ps'          = if inCheckpoint then ps else p : ps

    initialize :: a -> Maybe a -> Maybe a
    initialize x m = case m of
      Nothing -> Just x
      Just _  -> m

data SearchState = SearchState { searchStatePodState           :: PodState
                               , searchStateFirstMovement      :: Maybe ESPPodMovement
                               , searchStateNChecrkPointPassed :: Int
                               } deriving (Show)

data ESPPodMovement = ESPPodMovement { podMovementTurnAngle :: Double
                                     , podMovementThrust    :: Thrust
                                     } deriving (Show)

-- {-# LANGUAGE TypeSynonymInstances #-}
-- import GameRule
-- import Control.Parallel.Strategies


{- --- Just for Look up ~~
data GAGuess = GAGuesss{seed::Bool,
                     angRes::Bool
                     geneLength::Int,
                     popSize::Int,
                     pSwap  :: Double, 
                     pMutate::Double,
                     pCross::Double,
                     select::Int -> Population -> IO Population,
                     crossover ::Int->Double->Gene->Gene->IO (Gene,Gene)
                     mutation  ::Bool -> Int->Double->Gene-> IO Gene
                    }

-}
-------- Parameters
defaultGAGuess = GAGuess{oppoSeed=False,
                         selfSeed=False, --- do we put artifitial seed in the initial Population?
                         angRes=False, --- do we divide random generated angle range (-18,18) into 3 or 5 part  
                         geneLength=6,
                         oppoPopSize=30,
                         selfPopSize=60,
                         pSwap =0.07,
                         pMutate=0.8,
                         pCross=0.9, 
                         select=defaultSelect, 
                         crossover=defaultCross, 
                         mutation = defaultMutate,
                         oppoMaxT = 17%1000,
                         selfMaxT = 53%1000
                      }
                       
boostPenalty = 12000 :: Double
podScoreCkptWeight = 16000 :: Int
teamscoreMeasureSelfOppoRate = 1

--------- Types
-- | In each step the pod turns between [-18,+18] degrees 
type DeltaAngle = Angle
-- | Step is an action to take in one step
type Step = ( DeltaAngle , Thrust )
-- | Genotype of the realTime GA is a list of action to take in the following steps 
type  Gene = [Step]
-- | A population is a list of (Gene for Pod1 , Gene for Pod2 ) sorted by fitness (the head is the best individual)  
type Population = [(Gene,Gene)]



-------------CrossOver 

defaultCross ::Int->Double ->  Gene -> Gene -> IO (Gene,Gene)
defaultCross  geneLength pCross  g1 g2 = do
  n <- randomIO :: IO Double
  k <- randomIO :: IO Double
  if n>pCross then return (g1,g2) else do
    crossPoint <- randomRIO (1,geneLength-1)
    let g1' = take crossPoint g1 ++ drop crossPoint g2
    let g2' = take crossPoint g2 ++ drop crossPoint g1
    return (midValues k g1' g2',midValues k g2' g1')
    where
      midValues :: Double -> [Step] -> [Step] -> [Step]
      midValues k  = zipWith (\(ang1, th1) (ang2,th2)->case () of
                                 _
                                   | Normal t1 <- th1,Normal t2 <- th2
                                     -> (ang1*k + ang2*(1-k),Normal $ round $ (fromIntegral t1*k)+fromIntegral t2*(1-k))
                                   | otherwise
                                     -> (ang1*k + ang2*(1-k),th1)
                             )

-------------Selection
defaultSelect :: Int -> Population -> IO Population
defaultSelect popSize ps = randomPerm $ take popSize $ cycle $ take (popSize `div` 2) ps  

------------- Random Population

randomPop  :: Bool -> Int -> Int -> IO Population
randomPop angRes geneLength popSize =
  let randomPair = (,) <$> randomGene angRes geneLength <*> randomGene angRes geneLength
  in  sequence $ replicate popSize randomPair

randomGene :: Bool -> Int -> IO Gene
randomGene angRes geneLength = sequence $ replicate geneLength $ randomStep angRes

randomStep :: Bool -> IO Step
randomStep angleRes = do
  let res = if angleRes then 1 else 2
  i <- randomRIO ((-res),res) ::IO Int
  let delAngle = (18/fromIntegral res)*(fromIntegral i)
  n        <- randomRIO (0,59) :: IO Double                
  let thrust = case n of                                
        _ | 0<=n  && n<10 -> Normal 0
          | 10<=n && n<20 -> Normal 50
          | 20<=n && n<30 -> Normal 100                 
          | 30<=n && n<40 -> Normal 200                 
          | 40<=n && n<50 -> Boost                      
          | 50<=n && n<60 -> Shield            
  return (delAngle,thrust)

--------------- Mutation

defaultMutate ::Bool -> Int-> Double -> Gene -> IO Gene
defaultMutate angRes  geneLength pMutate  g = do
  n <- randomIO :: IO Double
  if n>pMutate then return g else do 
    mutatePoint <- randomRIO (0,geneLength-1)
    let (oldAng, oldThrust) = g!!mutatePoint  
    (newAng,newThrust) <- randomStep angRes 
    angOrThrust <- randomIO :: IO Bool
    let newStep = if angOrThrust then (newAng,oldThrust) else (oldAng,newThrust)
    return (take mutatePoint g ++ [newStep] ++ drop (mutatePoint+1) g)  

------------- Fitness 

fitness   :: Int -> [PodState]-> Driver->(Gene,Gene) -> Double 
fitness geneL initialState oppoDriver genePair  =
  let simResult = simTeam geneL geneL(makeDriver genePair , oppoDriver) initialState
  in  measureTeam simResult

-- | take step number and a list of 2 PodStates and update the "PodMovement" of the two pods
type Driver = Int -> [PodState]->[PodState]
defaultOpponent :: Driver  
defaultOpponent _  = map defaultDriver 

-- | default Driver , just move toward nextckpt
defaultDriver ::  PodState -> PodState
defaultDriver ps@PodState{podPosition = pos,podAngle = ang,podNextCheckPoints=cs} =
  let thValue =  if isJust ang then if abs (normalizeDeg $ (radToDeg $  arg (head cs - pos)) - (radToDeg.fromJust) ang )>90 then 0  else 100 else 100
  in if length cs > 0 then ps{podMovement = PodMovement (head cs) (Normal thValue)} else ps 
-- | make Driver from given Gene
makeDriver :: (Gene,Gene) -> Driver
makeDriver (g1,g2) n [p1,p2] =
  [p1{podMovement = decodeStep p1 (g1!!n)},p2{podMovement = decodeStep p2 (g2!!n)}]

simTeam :: Int -> Int -> (Driver,Driver) -> [PodState] -> [PodState]
simTeam _ 0 (player ,opponent) psNow = psNow
simTeam geneLen n (player ,opponent) psNow =
  let ps' = player (geneLen - n) (take 2 psNow) ++ opponent (geneLen - n) (drop 2 psNow)
  in  simTeam geneLen (n-1) (player,opponent) $ gameSimTurns 1 ps'

-- | Calculate the fitness of a (Gene,Gene) for (pod1,pod2)
measureTeam :: [PodState] -> Double 
measureTeam  [p1,p2,o1,o2] =
 let pMax = max (measurePod p1) (measurePod p2)
     oMax = max (measurePod o1) (measurePod o2)
     podMin = if measurePod p1 == pMax then  p2 else  p1
     oppoMax = if oMax == measurePod o1 then o1 else o2
     podMax = if measurePod p1 == pMax then  p1 else  p2
     podMaxPos = podPosition podMax
     podMinPos = podPosition podMin
     oppoMaxPos = podPosition oppoMax
     oppoCkpt2 = if length (podNextCheckPoints oppoMax) >1 then head$tail$podNextCheckPoints oppoMax else oppoMaxPos
     oppoCkpt1 = if length (podNextCheckPoints oppoMax) >0 then head$podNextCheckPoints oppoMax else oppoMaxPos
     waitPoint1 = ((200 `scalarMul` normalize (oppoMaxPos - oppoCkpt1)) + oppoCkpt1)
     waitPoint2 = ((200 `scalarMul` normalize (oppoCkpt1 - oppoCkpt2)) + oppoCkpt2)
     faceOppoLoss = abs $ arg (oppoMaxPos - podMinPos) - (fromJust $ podAngle podMin)
     podMinScore
       | pMax - oMax  > 700
       = (-0.4) * norm (podMinPos - podMaxPos)
       |  norm (podMinPos - waitPoint1)< 400
       = (-500) * faceOppoLoss 
       |  norm (podMinPos -waitPoint2) < 400
       = (-500) * faceOppoLoss
       | norm (oppoMaxPos - waitPoint1)> norm (podMinPos - waitPoint1) + 1500
       = (-0.4) * norm (podMinPos - waitPoint1) + (-300)*faceOppoLoss  
       | otherwise
       = (-0.4) * norm (podMinPos - waitPoint2) + (-300)*faceOppoLoss  
 in
   (1.5*pMax) - (1.5*oMax) 
   - (if ( (podBoostAvail p1) == False) then boostPenalty else 0)
   - (if ( (podBoostAvail p2) == False) then boostPenalty else 0)
   + podMinScore                                                                  
                                                                     
measurePod ::  PodState -> Double
measurePod PodState{podPosition=pos,podAngle = (Just ang),podNextCheckPoints = ckpts} =
  let len = length ckpts
      dist = if len > 0 then norm ((pos+(250`scalarMul`unitVec ang)) -head ckpts) else 0
  in  ( negate $ fromIntegral ( podScoreCkptWeight * len)) - dist
  
-------------Define Player

-- | A simple GA player (no configuring parameter for now)

data GAGuess = GAGuess{oppoSeed::Bool,
                       selfSeed::Bool,
                     angRes::Bool,
                     geneLength::Int,
                     oppoPopSize::Int,
                     selfPopSize::Int,
                     pSwap::Double,
                     pMutate::Double,
                     pCross::Double,
                     select::Int -> Population -> IO Population,
                     crossover ::Int->Double->Gene->Gene->IO (Gene,Gene),
                     mutation  ::Bool -> Int->Double->Gene-> IO Gene,
                     oppoMaxT :: Rational,
                     selfMaxT :: Rational 
                    }

instance PlayerIO GAGuess where
  playerInitIO  = return 
  playerRunIO playerParam@(GAGuess oppoSeed selfSeed angRes  geneLength oppoPopSize selfPopSize _ _ _ _ _ _ oppT selfT) PlayerIn{selfPod=[p1,p2],oppoPod=[o1,o2]} = do
-- Evolve Opponent Behaviour
    t0 <- getCurrentTime
    let oppoDefaultSeed = (defaultGene geneLength o1 ,defaultGene geneLength o2)
    oppoRand <-randomPop angRes geneLength (if oppoSeed then (oppoPopSize -2) else oppoPopSize)
    let oppoG0 = sortOn (negate.fitness geneLength [o1,o2,p1,p2] defaultOpponent) $ if oppoSeed then oppoDefaultSeed :oppoDefaultSeed:oppoRand else oppoRand
    oppoFinal <- evolve playerParam oppoPopSize t0 oppT [o1,o2,p1,p2] defaultOpponent oppoG0 0
    let oppoDriver = makeDriver $ head oppoFinal

-- Evolve player according to Opponent Behaviour
    t1 <- getCurrentTime
    -- Initialize first generation (add default seed or not) 
    let selfDefaultSeed = (defaultGene geneLength p1 ,defaultGene geneLength p2)
    let randSize = if selfSeed then (selfPopSize-2) else selfPopSize
    selfRand <- randomPop angRes  geneLength randSize
    let selfG0 = sortOn (negate.fitness geneLength [p1,p2,o1,o2] oppoDriver) $ if selfSeed then selfDefaultSeed:selfDefaultSeed:selfRand else selfRand

    -- evolve player 
    popFinal <- evolve playerParam selfPopSize t1 selfT [p1,p2,o1,o2] oppoDriver selfG0 0
    return (decodeGene (p1,p2) $  head popFinal ,playerParam)
    where
      decodeGene :: (PodState,PodState) -> (Gene,Gene) -> [PodMovement]
      decodeGene (ps1,ps2) (g1,g2) = [decodeStep ps1 $ head g1 , decodeStep ps2 $ head g2]

evolve :: GAGuess -> Int  -> UTCTime -> Rational -> [PodState] -> Driver -> Population -> Int -> IO Population  
evolve playerParam@(GAGuess _ _ angRes geneLength _ _ pSwap pMutate pCross select crossover mutate _ _) popSize t0 tMax podStates oppoDriver g0 gCnt = do
  tc <- getCurrentTime
  if toRational (tc `diffUTCTime` t0) > tMax then return $ trace (show gCnt) g0 else do
    let [p1,p2] = take 2 g0
    matingPool  <- select popSize g0
    childrens   <- crossoverAndMutate matingPool
    let fits =  parMap rpar (fitness geneLength podStates oppoDriver) (p1:p2:childrens)
    let g1 = take popSize $ sortOn (negate.fitness geneLength podStates oppoDriver) (p1:p2:childrens)
    evolve playerParam  popSize t0 tMax podStates oppoDriver g1 (gCnt+1) 
    where
      crossoverAndMutate :: Population -> IO Population -- length of population should be even 
      crossoverAndMutate  (g1@(g11,g12):g2@(g21,g22):gRest) = do 
        gRest'' <- crossoverAndMutate gRest 
        s  <- randomIO :: IO Double -- decide either swap-team-members or cross+mutate  
        if s<pSwap then return  ((g11,g22):(g21,g12):gRest'') else do 
          whichGene <- randomIO :: IO Bool  -- decide which pair of players to  cross and mutate
          if whichGene then do
            (g11' ,g21')<- crossover geneLength pCross g11 g21
            [g11'', g21''] <- sequence $ map (mutate angRes geneLength pMutate) [g11',g21']
            return ((g11'',g12):(g21'',g22):gRest'')
            else do
            (g12' ,g22')<- crossover geneLength pCross g12 g22
            [g12'', g22''] <- sequence $ map (mutate angRes geneLength pMutate) [g12',g22']
            return ((g11,g12''):(g21,g22''):gRest'')                                                  
      crossoverAndMutate  [] =return  []
    
-- | Decode a Gene as PodMovement
decodeStep :: PodState -> Step -> PodMovement
decodeStep PodState{podPosition=pos,podAngle = podAng,podNextCheckPoints = ckpts} (ang,thrust)
  | Just angBase <- podAng = PodMovement (pos + (5000 `scalarMul` unitVec (degToRad ang+angBase) )) thrust
  | Nothing      <- podAng = PodMovement (head ckpts) thrust

  
-- Default Gene
encode :: PodState -> PodMovement -> Step
encode PodState{podPosition=pos,podAngle=ang} PodMovement{podTarget=target,podThrust = thrust}
  = let tarAng = radToDeg $ arg $ (target-pos)
        turnAng= clamp (-18) (normalizeDeg (tarAng - (radToDeg.fromJust) ang)) (18)
    in (turnAng,thrust)

defaultGene :: Int -> PodState-> Gene
defaultGene geneL  ps =  
  let 
      iterFunc = (\x ->  (speedDecay $driftPod 1$ thrustPod $ rotatePod $ defaultDriver x ))
      defaultDriverHistory = iterate iterFunc ps
  in map (\ps -> encode ps (podMovement ps)) $ take geneL $ tail defaultDriverHistory 
         








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
distinctPairs xs = concat $ map (\n-> zip (repeat (xs!!n)) (drop (n+1) xs)) [0..(length xs-2)]
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

normalizeRad :: Double -> Double
normalizeRad x = ((x + pi) `fmod` (2 * pi)) - pi

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

normalizeDeg :: Double -> Double
normalizeDeg x = x - ((fromIntegral.round) (x/360) * 360)  

isRepOf :: (Eq a) => [a] -> [a] -> Bool
isRepOf _ [] = True
isRepOf needle haystack
  | p1 == needle = isRepOf needle p2
  | otherwise = False
  where (p1, p2) = splitAt (length needle) haystack

getMinRepLen :: (Eq a) => [a] -> Int
getMinRepLen xs = n
  where Just n = find (\n -> isRepOf (take n xs) xs) [1..]
