{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns#-}
module GameSim
  (
    PodState(..)
  , PodMovement(..)
  , Angle(..)
  , Thrust(..)
  , gameSimTurn
  )
where

import Data.Maybe
import Data.List
import Data.Vec2(Vec2(..))
import qualified Data.Vec2 as V
import qualified Util as U

type Angle = Double

data PodState = PodState { podPosition          :: !Vec2
                         , podSpeed             :: !Vec2
                         , podAngle             :: !Angle
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




-- | simulate Game Phisic for one turn
gameSimTurn :: [PodState] -> [PodState]
gameSimTurn pss = map speedDecay $ movePods $ map (thrustPod.rotatePod) pss


-- | Rotate Pods (change angle)
rotatePod :: PodState -> PodState
rotatePod ps@(PodState position _ angle _ _ (PodMovement target thrust) _ )
  = let deltaAngle = normalize (V.arg (target - position) - angle)
        normalize !th
          | th > pi  = th - 2*pi
          | th <= pi = th + 2*pi
          | otherwise = th
        r = U.maxTurnAngle -- range of turning angle
        angle' = angle + (U.clamp (-r) deltaAngle r)
    in  ps{podAngle = angle'}
         

-- | Update the PodState ( speed , boost , shield ) according to current PodMovement 
thrustPod :: PodState -> PodState 
thrustPod ps@(PodState position speed angle boostAvail shieldState (PodMovement target thrust) _ )
   = let
         shieldState' = shieldNextState (thrust == Shield) shieldState
         idle = isJust shieldState'         
         accMag = if idle then 0 else
           case thrust of
             Normal n -> fromIntegral $ U.clamp 0 n 100 
             Boost    -> if boostAvail then U.boostAccel else 100
         acc  = accMag  `V.scalarMul` (V.unitVec angle)
         speed'= if idle then speed else speed + acc
         boostAvail' = boostAvail && (thrust /= Boost)
      in
         ps{podSpeed=speed',podBoostAvail=boostAvail',podShieldState = shieldState'}

-- | list index of a pod
type PodID = Int
-- | represent time in a single term  (between 0 and 1)
type Time  = Double

-- | Move Pod according to PodState (Including Collision)
movePods :: [PodState] -> [PodState]
movePods pss = movePods' 0 pss 
  where
        movePods' :: Time -> [PodState] -> [PodState]
        movePods' currentT pss 
          | isNothing $ firstCollision pss = map (driftPod (1-currentT)) pss
          | Just(_,_,collideT)<-firstCollision pss , (collideT+currentT)>1 = map (driftPod (1-currentT)) pss
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
      position' =  position + (dt `V.scalarMul` speed)
      radius = U.podForceFieldRadius + U.checkPointRadius
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
     foldl' folder Nothing  [ collideDetect pss i1 i2 | (i1,i2) <- U.distinctPairs [0..(length pss -1)] ]

  
-- | Detect the time before collision of 2 Pod
collideDetect :: [PodState] -> PodID -> PodID -> Maybe ( PodID , PodID , Time )
collideDetect pss i1 i2 = 
  let (p1,p2) = (podPosition (pss!!i1),podPosition (pss!!i2))
      (v1,v2) = (podSpeed    (pss!!i1),podSpeed    (pss!!i2))

      -- fix the origin of the coordinate to p1
      p2' = p2 - p1
      v2' = v2 - v1
      radius = U.podForceFieldRadius * 2
      boomTime = collideTime p2' v2' radius
  in
      fmap (\t->(i1,i2,t)) boomTime   

-- | Find the collision time with  origin
collideTime :: Vec2 -> Vec2 -> Double -> Maybe Time
collideTime position speed  radius =
  let nearest = position - (position `V.proj` speed)
      minDistSquare = nearest `V.dot` nearest
      radiusSquare =  radius * radius
      -- below work only if minDist < radius , otherwise "sqrt" return error
      distBeforeCollide = (V.norm (position `V.proj` speed)) - sqrt (radiusSquare - minDistSquare)
      timeBeforeCollide = distBeforeCollide / V.norm speed
  in
      case () of
        _
          -- the point is moving further away
          | speed `V.dot` position > 0 -> Nothing
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
      impact = impactCoefficiant `V.scalarMul` (v2' `V.proj` p2')
      -- if the impact vector is shorter than minima , normalize it  
      impact' = if V.norm impact < U.podMinCollisionImpact
        then ( U.podMinCollisionImpact /V.norm impact) `V.scalarMul` impact
        else impact
      pod1' = pod1{podSpeed = v1 + impact + impact'}
      pod2' = pod2{podSpeed = v2 - impact - impact'}
      replace i x xs = take i xs ++ [x] ++ drop (i+1) xs
  in  
      replace i1 pod1' $ replace i2 pod2' pss
-- | Decay speed of a pod due to friction   
speedDecay :: PodState -> PodState
speedDecay ps@PodState{podSpeed = speed} = ps{podSpeed = 0.85 `V.scalarMul` speed}



-----------TESTING UTILITY-------------

zeroV = Vec2 0 0 

pos :: PodState -> Vec2
pos p = podPosition p

zeroPod = PodState zeroV zeroV 0 True Nothing (PodMovement zeroV (Normal 0)) []

drifter :: Vec2 -> Vec2 -> PodState
drifter pos speed = zeroPod{podPosition = pos,podSpeed = speed}

mover  :: Vec2 -> Vec2  -> PodState
mover   pos speed = let ans = (drifter pos speed){podMovement = PodMovement (podPosition ans + podSpeed ans) (Normal 100)}
  in ans


roundn :: [PodState]->Int->[PodState]
roundn init n =  (!!n) $ iterate gameSimTurn init

rounds :: [PodState]->Int-> IO()
rounds init n = mapM_ (\x->putStrLn $show x) $ take n $ (map (map pos) $iterate gameSimTurn init)


pod1 = drifter (Vec2 1000 0) (Vec2 100 0)
pod2 = drifter (Vec2 2000 0) (Vec2 (-100) 0)

game = [pod1,pod2]
