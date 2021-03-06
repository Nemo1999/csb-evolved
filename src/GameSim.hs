{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns#-}
{-# OPTIONS_GHC -O2  #-}
module GameSim
  (
    Time
  , PodState(..)
  , PodMovement(..)
  , Angle(..)
  , Thrust(..) 
  , rotatePod
  , thrustPod
  , movePods
  , driftPod
  , speedDecay
  , gameSimTurns
  ,shieldNextState
  , roundTrunc
  )
where

import Data.Maybe
import Data.List
import qualified Data.Vector as V  
import Data.Vec2(Vec2(..))
import Data.Vec2 
import qualified Util as U

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
          r = U.maxTurnAngle -- range of turning angle
          angle' = normalize $ (angle + (U.clamp (-r) deltaAngle r))
      in  ps{podAngle = Just angle'}
         

-- | Update the PodState ( speed , boost , shield ) according to current PodMovement 
thrustPod :: PodState -> PodState 
thrustPod ps@(PodState position speed (Just angle) boostAvail shieldState (PodMovement target thrust) _ )=
     let
         shieldState' = shieldNextState (thrust == Shield) shieldState
         idle = isJust shieldState'         
         accMag = if idle then 0 else
           case thrust of
             Normal n -> fromIntegral $ U.clamp 0 n 200 
             Boost    -> if boostAvail then U.boostAccel else 200
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
      radius = U.checkPointRadius-30
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
     foldl' folder Nothing  [ collideDetect (p1,p2) | (p1,p2) <- U.distinctPairs pss ]

  
-- | Detect the time before collision of 2 Pod
collideDetect :: (PodState,PodState)-> Maybe ( (PodState , PodState) , Time )
collideDetect pair@(pod1,pod2) = 
  let (p1,p2) = (podPosition (pod1),podPosition (pod2))
      (v1,v2) = (podSpeed    (pod1),podSpeed    (pod2))
      -- fix the origin of the coordinate to p1
      p2' = p2 - p1
      v2' = v2 - v1
      radius = U.podForceFieldRadius * 2
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
      impact' = if norm impact < U.podMinCollisionImpact
        then ( U.podMinCollisionImpact /norm impact) `scalarMul` impact
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
