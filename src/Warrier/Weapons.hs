{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE PatternGuards #-}
module Warrier.Weapons where

{-
import Util
import Data.Vec2
import GameSim
-- import GameRule
import Player
import Player.GA
-}

import Data.Array.IO
import Data.Maybe
import Data.List
import System.Random
import System.IO
import Control.Monad
import System.Timeout
import System.CPUTime
import Debug.Trace
--import Control.Parallel

----Vec2------------------------
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
  

----Util------------------------------------------------
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


----GameSimulation------------------------------------------------

type Angle = Double

data PodState = PodState { podPosition          :: !Vec2
                         , podSpeed             :: !Vec2
                         , podAngle             :: !(Maybe Angle)
                         , podBoostAvail        :: !Bool
                         , podShieldState       :: !ShieldState
                         , podMovement          :: !PodMovement
                         , podNextCheckPoints   :: ![Vec2]
                         } deriving (Show)


data PodMovement = PodMovement { podTarget :: Vec2
                               , podThrust :: Thrust
                               } deriving (Show, Eq)


data Thrust = Normal Int | Shield | Boost deriving (Show,Eq)



type ShieldState = Maybe Int 
shieldNextState :: Bool -> ShieldState -> ShieldState
shieldNextState activated ss = if activated then Just 3 else
                                          case ss of
                                            Just n -> if n>0 then Just (n-1) else Nothing
                                            Nothing -> Nothing




-- | simulate Game Phisic for n turn
gameSimTurns ::Int -> [PodState] -> [PodState]
gameSimTurns 0 !pss = pss
gameSimTurns n !pss = gameSimTurns (n-1) $ map speedDecay $ movePods 1 $ map (thrustPod.rotatePod) pss
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
             Normal n -> fromIntegral $ clamp 0 n 100 
             Boost    -> if boostAvail then boostAccel else 100
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
          | Just(_,_,collideT)<-firstCollision pss,(collideT+currentT)>duration = map (driftPod (duration-currentT)) pss        -- Collision happens
          | Just(i1,i2,collideT)<-firstCollision pss =
              let result1 = map (driftPod collideT) pss
                  result2 = collide2Points i1 i2 result1
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
      radius = checkPointRadius
      reachTime = collideTime reletivePos speed radius
      ckpts'
        | ckpts == []  = []
        | Just t <-reachTime , t  < dt =  tail ckpts
        | otherwise = ckpts
  in
      pod{podPosition =position',podNextCheckPoints = ckpts'}
      


-- | Find the first upcoming collision time and pair of Pods
firstCollision :: [PodState]  -> Maybe ( PodID , PodID , Time )  
firstCollision pss  =
  let folder prev new
        | Nothing  <- prev    =  new
        | Nothing  <- new     =  prev
        | Just (_,_,t') <- new , Just (_,_,t) <-prev = if t' < t then new else prev
  in
     foldl' folder Nothing  [ collideDetect pss i1 i2 | (i1,i2) <- distinctPairs [0..(length pss -1)] ]

  
-- | Detect the time before collision of 2 Pod
collideDetect :: [PodState] -> PodID -> PodID -> Maybe ( PodID , PodID , Time )
collideDetect pss i1 i2 = 
  let (p1,p2) = (podPosition (pss!!i1),podPosition (pss!!i2))
      (v1,v2) = (podSpeed    (pss!!i1),podSpeed    (pss!!i2))

      -- fix the origin of the coordinate to p1
      p2' = p2 - p1
      v2' = v2 - v1
      radius = podForceFieldRadius * 2
      boomTime = collideTime p2' v2' radius
  in
      fmap (\t->(i1,i2,t)) boomTime   

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
collide2Points :: PodID -> PodID -> [PodState] -> [PodState]
collide2Points i1 i2 pss =
  let (pod1,pod2) = (pss!!i1 , pss!!i2)
      (p1,p2,v1,v2) = (podPosition pod1 , podPosition pod2 , podSpeed pod1 , podSpeed pod2)
      -- move the origin of coordinate to p1
      (p1',v1') = (Vec2 0 0 , Vec2 0 0)
      (p2',v2') = (p2 - p1 , v2 - v1 )
      --the mass of the pod
      m1 = if (podShieldState  pod1) == Just 3 then 10 else 1
      m2 = if (podShieldState  pod2) == Just 3 then 10 else 1
      -- the impact is the
      impactCoefficiant = ((m1*m2)/(m1+m2))
      impact = impactCoefficiant `scalarMul` (v2' `proj` p2')
      -- if the impact vector is shorter than minima , normalize it  
      impact' = if norm impact < podMinCollisionImpact
        then ( podMinCollisionImpact /norm impact) `scalarMul` impact
        else impact
      pod1' = pod1{podSpeed = v1 + impact + impact'}
      pod2' = pod2{podSpeed = v2 - impact - impact'}
      replace i x xs = take i xs ++ [x] ++ drop (i+1) xs
  in  
      replace i1 pod1' $ replace i2 pod2' pss
-- | Decay speed of a pod due to friction   
speedDecay :: PodState -> PodState
speedDecay ps@PodState{podSpeed = speed} = ps{podSpeed = 0.85 `scalarMul` speed}



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

----Player------------------------------------------------
data PlayerIn = PlayerIn {selfPod ::[PodState]
                         ,oppoPod ::[PodState]} deriving Show

type PlayerOut = [PodMovement]

class Player p where
    playerInit :: p -> p  
    playerRun  :: p -> PlayerIn -> (PlayerOut, p)
    
class PlayerIO p where
    playerInitIO :: p -> IO p
    playerRunIO  :: p -> PlayerIn -> IO (PlayerOut , p)
    
newtype WrapIO p = WrapIO p

-- | every Player p can be used as PlayerIO p     
instance (Player p) => PlayerIO (WrapIO p) where  
  playerInitIO (WrapIO !p)  = return  $ WrapIO $ playerInit p
  playerRunIO  (WrapIO !p)  !pin =
    let (!pout, !p') = playerRun p pin  in return (pout, WrapIO p')

----GA------------------------------------------------
-------- Parameters
geneLength = 8 :: Int
popSize = 16 :: Int 
pCross = 0.8 :: Double
pMutate = 0.8 :: Double
boostPenalty = 10000 :: Double
podScoreCkptWeight = 16000 :: Int
teamscoreMeasureSelfOppoRate = 1
maxTime = 60000000000 ::Integer -- maximum time before returning the final answer 

--------- Types
-- | In each step the pod turns between [-18,+18] degrees 
type DeltaAngle = Angle
-- | Step is an action to take in one step
type Step = ( DeltaAngle , Thrust )
-- | Genotype of the realTime GA is a list of action to take in the following steps 
type  Gene = [Step]
-- | A population is a list of (Gene for Pod1 , Gene for Pod2 ) sorted by fitness (the head is the best individual)  
type Population = [(Gene,Gene)]

-- | PodScore is used to calculate fitness 
-- | The higher the score , the closer the Destination
-- | Int is the length of the up comming checkpoints
-- | double is the distance to the next checkpoint
data PodScore =  PodScore (Int,Double) deriving Eq
instance Ord PodScore where
  compare ps1 ps2 =
    compare  (measurePodScore  ps1)
             $ measurePodScore  ps2

-- | The Score of a simulation result of a pair of Genes
-- | Contains the PodScore of player's best Pod and opponent's best Pod  
data  TeamScore = TeamScore{selfBest::PodScore , oppoBest::PodScore} deriving Eq
instance Ord TeamScore where
  compare ts1 ts2
    = compare  (measureTeamScore 1 ts1) $ measureTeamScore 1 ts2

-------------CrossOver 

crossover ::Int ->  Gene -> Gene -> IO (Gene,Gene)
crossover geneLength g1 g2 = do
  n <- randomIO :: IO Double
  k <- randomIO :: IO Double
  if n>pCross then return (g1,g2) else do
    crossPoint <- randomRIO (1,geneLength-1)
    let g1' = take crossPoint g1 ++ drop crossPoint g2
    let g2' = take crossPoint g2 ++ drop crossPoint g1
    return (midValues k g1' g2',midValues k g2' g1')
    where
      midValues k  = zipWith (\(ang1,th1) (ang2,th2)->(ang1*k + ang2*(1-k),th1)) 

------------- Random Population

randomPop  :: Int -> Int -> IO Population
randomPop geneLength popSize =
  let randomPair = (,) <$> randomGene geneLength <*> randomGene geneLength
  in  sequence $ replicate popSize randomPair

randomGene :: Int -> IO Gene
randomGene geneLength = sequence $ replicate geneLength randomStep

randomStep :: IO Step
randomStep = do
  delAngle <- randomRIO (-18.0,18.0)::IO Double 
  n        <- randomRIO (0,49) :: IO Double                
  let thrust = case n of                                
        _ | 0<=n  && n<10 -> Normal 0                   
          | 10<=n && n<20 -> Normal 50                 
          | 20<=n && n<30 -> Normal 100                 
          | 30<=n && n<40 -> Boost                      
          | 35<=n && n<50 -> Shield                     
  return (delAngle,thrust)

--------------- Mutation

mutate :: Int -> Gene -> IO Gene
mutate geneLength g = do
  n <- randomIO :: IO Double
  if n>pMutate then return g else do 
    mutatePoint <- randomRIO (0,geneLength-1)
    let (oldAng, oldThrust) = g!!mutatePoint  
    (newAng,newThrust) <- randomStep
    angOrThrust <- randomIO :: IO Bool
    let newStep = if angOrThrust then (newAng,oldThrust) else (oldAng,newThrust)
    return (take mutatePoint g ++ [newStep] ++ drop (mutatePoint+1) g)  

------------- Fitness 

fitness   :: [PodState]-> (Gene,Gene) -> Double 
fitness initialState genePair =
  let simResult = simTeam geneLength (makeDriver genePair , defaultOpponent) initialState
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

simTeam :: Int -> (Driver,Driver) -> [PodState] -> [PodState]
simTeam 0 (player ,opponent) psNow = psNow
simTeam n (player ,opponent) psNow =
  let ps' = player (geneLength-n) (take 2 psNow) ++ opponent (geneLength - n) (drop 2 psNow)
  in  simTeam (n-1) (player,opponent) $ gameSimTurns 1 ps'

-- | Calculate the fitness of a (Gene,Gene) for (pod1,pod2)
measureTeam :: [PodState] -> Double 
measureTeam  [p1,p2,o1,o2] =
 let pMax = max (getPodScore p1) (getPodScore p2)
     oMax = max (getPodScore o1) (getPodScore o2)
 in  (measureTeamScore teamscoreMeasureSelfOppoRate $ TeamScore pMax oMax)  
     - if (podThrust (podMovement p1) == Boost) then boostPenalty else 0
     - if (podThrust (podMovement p2) == Boost) then boostPenalty else 0
     + (0.5*(measurePodScore $ min (getPodScore p1)(getPodScore p2)))

-- | turn TeamScore into Double for compare
-- | the higher the score , the better the team
measureTeamScore :: Double -> TeamScore -> Double
measureTeamScore selfOppoRate TeamScore{selfBest=s,oppoBest = o} =
    selfOppoRate * measurePodScore s +  measurePodScore o  
 
-- | turn PodScore into Double for compare
measurePodScore ::  PodScore -> Double
measurePodScore (PodScore (i,d))=
  ( negate $ fromIntegral ( podScoreCkptWeight * i)) - d
-- | Evalutating the score of a single  PodState
getPodScore :: PodState -> PodScore
getPodScore PodState{podPosition=pos,podNextCheckPoints = ckpts} =
  let len = length ckpts
      dist = if len > 0 then norm (pos-head ckpts) else 0
  in PodScore (len ,dist) 


-------------Define Player

-- | A simple GA player (no configuring parameter for now)

data GASimple = GASimple
instance PlayerIO GASimple where
  playerInitIO _ = return GASimple
  playerRunIO _ PlayerIn{selfPod=[p1,p2],oppoPod=[o1,o2]} = do
    t0 <- getCPUTime
    let defaultSeed = (defaultGene p1 ,defaultGene p2)

    --popRand <- randomPop geneLength (popSize)
    --let pop0 = sortOn (negate.fitness [p1,p2,o1,o2]) $  popRand
    
    popRand <- randomPop geneLength (popSize-2)
    let pop0 = sortOn (negate.fitness [p1,p2,o1,o2]) $  defaultSeed:defaultSeed:popRand
    
    popFinal <- evolve t0 [p1,p2,o1,o2] pop0 0
    return (decodeGene (p1,p2) $  head popFinal , GASimple)
    where
      
      decodeGene :: (PodState,PodState) -> (Gene,Gene) -> [PodMovement]
      decodeGene (ps1,ps2) (g1,g2) = [decodeStep ps1 $ head g1 , decodeStep ps2 $ head g2]
      evolve :: Integer -> [PodState]-> Population -> Int -> IO Population 
      evolve t0 ps g0 gCnt= do
        t1 <- getCPUTime
        if (t1-t0)>maxTime then return $
          trace (show gCnt )-- ++" "++show (head g0))
                                                $ g0 else do
          mg1 <-(nextGen geneLength ps g0) -- timeout 10 ....
          case Just mg1 of
            Just  g1 -> evolve t0 ps g1 (gCnt+1)
            Nothing  -> return g0
      
      
-- | Decode a Gene as PodMovement
decodeStep :: PodState -> Step -> PodMovement
decodeStep PodState{podPosition=pos,podAngle = podAng,podNextCheckPoints = ckpts} (ang,thrust)
  | Just angBase <- podAng = PodMovement (pos + (5000 `scalarMul` unitVec (degToRad ang+angBase) )) thrust
  | Nothing      <- podAng = PodMovement (head ckpts) thrust

  
nextGen :: Int -> [PodState] -> Population -> IO Population
nextGen geneLength  podStates ps = do
  let [p1,p2] = take 2 ps
  matingPool <- select popSize ps 
  childrens  <- crossoverAndMutate geneLength matingPool
  return $ take popSize $ sortOn (negate.fitness podStates) (p1:p2:childrens)
    where
      select :: Int -> Population -> IO Population
      select n ps = randomPerm $ take n $ cycle $ take (n `div` 2) ps  
      crossoverAndMutate :: Int -> Population -> IO Population -- length population should be even 
      crossoverAndMutate geneLength ((g11,g12):(g21,g22):gs) = do
            (g11', g21') <- crossover geneLength g11 g21
            (g12', g22') <- crossover geneLength g12 g22 
            [g11'',g12'', g21'' , g22''] <- sequence $ map (mutate geneLength) [g11', g12', g21', g22']
            gs'' <- crossoverAndMutate geneLength gs
            return ((g11'',g12''):(g21'',g22''):gs'')
      crossoverAndMutate geneLength [] =return  []

-- Default Gene
encode :: PodState -> PodMovement -> Step
encode PodState{podPosition=pos,podAngle=ang} PodMovement{podTarget=target,podThrust = thrust}
  = let tarAng = radToDeg $ arg $ (target-pos)
        turnAng= clamp (-18) (normalizeDeg (tarAng - (radToDeg.fromJust) ang)) (18)
    in (turnAng,thrust)

defaultGene :: PodState-> Gene
defaultGene ps =
  let 
      iterFunc = (\x ->  (speedDecay $driftPod 1$ thrustPod $ rotatePod $ defaultDriver x ))
      defaultDriverHistory = iterate iterFunc ps
  in map (\ps -> encode ps (podMovement ps)) $ take geneLength $ tail defaultDriverHistory 
         

------------ | copied from GameRule------------------------
emptyMovement = PodMovement zeroVec $ Normal 0
emptyPodState = PodState zeroVec zeroVec Nothing True Nothing emptyMovement []

----------------Warrier Start------------------------
type Ckpts = [Vec2]

readPoint :: IO Vec2
readPoint = do
  [x,y] <- fmap (map read) $ words<$>getLine :: IO [Double]
  return $ Vec2 x y

readCkpts :: IO (Int , Ckpts)
readCkpts = do
  laps <- read<$>getLine :: IO Int
  ckptCount <- read<$>getLine :: IO Int 
  ckpts <- sequence $ replicate ckptCount readPoint
  return (laps,ckpts)

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

   
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    
    cInfo@(laps,ckpts) <- readCkpts
    pod1  <- readPod cInfo
    pod2  <- readPod cInfo
    opp1  <- readPod cInfo
    opp2  <- readPod cInfo
    
    player <- playerInitIO $ GASimple--WrapIO $  ElementaryPlayer ()
    let playerIn = PlayerIn [pod1,pod2] [opp1,opp2]
    ([move1,move2] , player' )  <- playerRunIO player playerIn
    let (pod1' ,pod2')= (pod1{podMovement = move1} ,pod2{podMovement=move2})
    let [podStart1,podStart2] = map updateShieldThrust [pod1',pod2']
    putMovement move1
    putMovement move2
    -- game loop
    gameCycles ckpts [podStart1,podStart2,opp1,opp2] player'
