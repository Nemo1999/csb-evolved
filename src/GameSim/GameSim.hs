{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns#-}
module Game.Simulation
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
import Data.Vec2(Vec2)
import qualified Data.Vec2 as V
import qualified Util as U

type Angle = Double

data PodState = PodState { podPosition          :: Vec2
                         , podSpeed             :: Vec2
                         , podAngle             :: Angle
                         , podBoostAvail        :: Bool
                         , podShieldState        :: ShieldState
                         , podMovement          :: PodMovement
                         } deriving (Show, Read)


data PodMovement = PodMovement { podTarget :: Vec2
                         , podThrust :: Thrust
                         } deriving (Show, Read)


data Thrust = Normal Int | Shield | Boost deriving (Show, Read,Eq)
| Maybe n <- shieldstate , Shield <- thrust = ps{ podShieldState = shieldNextState True shieldState}


data ShieldState = Maybe Int
shieldNextState :: Bool -> ShieldState -> ShieldState
shieldNextState activated ss = if activate then Just 3 else
                                          case ss of
                                            Just n -> if n>0 then Just (n-1) else Nothing
                                            Nothing -> Nothing




-- | simulate Game Phisic for one turn
gameSimTurn :: [PodState] -> [PodState]
gameSimTurn pss = map speedDecay $ movePods $ map (thrustPod.rotatePod) pss


-- | Rotate Pods (change angle)
rotatePod :: PodState -> PodState
rotatePod ps@(PodState position _ angle _ _ (PodMovement target thrust))
  = let deltaAngle = normalize (V.arg (target - position) - angle)
        normalize !th
          | th > pi  = th - 2*pi
          | th <= pi = th + 2*pi
          | otherwise = th
        r = U.maxTurnAngle
        angle' = angle + (U.clamp -r deltaAngle r)
    in  ps{podAngle = angle'}
         

-- | Update the PodState ( speed , boost , shield ) according to current PodMovement 
thrustPod :: PodState -> PodState 
thrustPod ps@(PodState position speed angle boostAvail shieldState (PodMovement target thrust))
   = let
         shieldState' = shieldNextState (thrust == Shield) shieldState
         idle = isJust shieldState'         
         accMag = if idle then 0 else
           case thrust of
             Normal n -> n 
             Boost    -> if boostAvail then U.boostAccel else 100
         acc  = accMag V.`scalarMul` (V.unitVec angle)
         speed'= if idle then speed else speed + acc
         boostAvail' = boostAvail && (thrust \= Boost)
      in
         ps{podSpeed=speed',podBoostAvail=boostAvail',podShieldState = shieldState'}

-- | list index of a pod
type PodID = Int
-- | represent time in a single term  (between 0 and 1)
type Time  = Double

-- | Move Pod according to PodState (Including Collision)
movePods :: [PodState] -> [Podstate]
movePods pss = movePods' 0 pss 
  where driftPod ::  Time -> PodState ->  PodState
        driftPod !dt p@{podSpeed = !vel,podPosition = !pos} = p{podPosition = pos + (dt V.`scalarMul` vel)}

        movePods' :: Time -> [PodState] -> [PodState]
        movePods' currentT pss 
          | isNothing $ firstCollision pss = map (driftPod (1-currentT)) pss
          | Just(_,_,collideT)<-firstCollision pss , (collideT+currentT)>1 = map (driftPod (1-currentT)) pss
          | Just(i1,i2,collideT)<-firstCollision pss =
              let result1 = map (driftPod collideT) pss
                  result2 = collide2Points i1 i2 result1
              in  movePods' (collideT+currenT) result2

-- |Find the first upcoming collision time and pair of Pods
firstCollision :: [PodState] -> Time -> Maybe ( PodID , PodID , Time )  
firstCollision pss duration =
  let folder prev new
        | Nothing  <- prev    =  new
        | Nothing  <- new     =  prev
        | Just (_,_,t') <- new , Just (_,_,t) <-prev = if t' < t then new else prev
  in
     foldl' folder Nothing  [ collideDetect i1 i2 | (i1,i2) <- U.distinctPairs [1..(length pss - 1)] ]

-- Detect the time before collision of 2 Point
collideDetect :: [PodState] -> PodID -> PodID -> Maybe ( PodID , PodID , Time )
collideDetect pss i1 i2
  let (p1,p2) = (podPosition (pss!!i1),podPosition (pss!!i2))
      (v1,v2) = (podSpeed    (pss!!i1),podSpeed    (pss!!i2))
      -- fix the origin of the coordinate to p1
      p1' = Vec2 0 0
      p2' = p2 - p1
      v1' = Vec2 0 0
      v2' = v2 - v1
      -- nearest point on the orbit of p2'
      nearest = p2' - (p2' V.`proj` v2')
      -- min distance between p2' and p1'
      minDistSquare = (nearest) V.`dot` (nearest)
      podRadiusSquare =  U.podForceFieldRadius *  U.podForceFieldRadius
      --calculate the contact point
      distBeforeCollide = (V.norm (p2' V.`proj` v2')) - sqrt (podRadiusSquare - minDistSquare)
      timeBeforeCollide = distBeforeCollide / V.norm v2'
  in
     case () of _
       -- p2' is moving further away
       | (p2' V.`dot` v2') > 0 -> Nothing
       -- nearest point  is not close enough
       | minDistSquare > minDistSquare -> Nothing
       -- p1 and p2 will collide
       | otherwise -> Just (i1 , i2 , timeBeforeCollide)

-- | given two pods p1 p2 touching eachother , update their speed after collision
collide2Points :: PodID -> PodID -> [PodState] -> [PodState]
collide2Points i1 i2 pss =
  let (pod1,pod2) = (pss!!i1 , pss!!i2)
      (p1,p2,v1,v2) = (podPosition pod1 , podPosition pod2 , podSpeed pod1 , podSpeed pod2)
      -- move the origin of coordinate to p1
      (p1',v1') = (Vec2 0 0 , Vec2 0 0)
      (p2',v2') = (p2 - p1 , v2 - v1 )
      --the mass of the pod
      m1 = if (podThrust $ podMovement p1) == Just 3 then 10 else 1
      m2 = if (podThrust $ podMovement p2) == Just 3 then 10 else 1
      -- the impact is the
      impactCoefficiant = ((m1*m2)/(m1+m2))
      impact = impactCoefficiant v.`scalarMul` (v2' V.`proj` p2')
      -- if the impact vector is shorter than minima , normalize it  
      impact' = if norm impact < U.podMinCollisionImpact
        then ( U.podMinCollisionImpact /V.norm impact) V.`scalarMul` impact
        else impact
      pod1' = pod1{podSpeed = v1 + impact + impact'}
      pod2' = pod2{podSpeed = v2 - impact - impact'}
      replace i x xs = take i xs ++ [x] ++ drop (i+1) xs
  in  
      replace i1 pod1' $ replace i2 pod2' pss
-- | Decay speed of a pod due to friction   
speedDecay :: PodState -> PodState
speedDecay ps{podSpeed = speed} = ps{podSpeed = 0.85 V.`scalarMul` speed}



 
