{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns#-}
module Game.Simulation
  (
    runGame
  , runGameIO
  , recordGame
  , recordGameIO
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


data Movement = Movement { podTarget :: Vec2
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
rotatePod ps@(PodState position _ angle _ _ (Movement target thrust))
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
thrustPod ps@(PodState position speed angle boostAvail shieldState (Movement target thrust))
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


type PodID = Int
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


firstCollision :: [PodState] -> Time -> Maybe ( PodID , PodID , Time )  
firstCollision pss duration =
  let folder prev new
        | Nothing  <- prev    =  new
        | Nothing  <- new     =  prev
        | Just (_,_,t') <- new , Just (_,_,t) <-prev = if t' < t then new else prev
  in
     foldl' folder Nothing  [ collideDetect i1 i2 | (i1,i2) <- U.distinctPairs [1..(length pss - 1)] ]

collideDetect :: [PodState] -> PodID -> PodID -> Maybe ( PodID , PodID , Time )
collideDetect pss i1 i2
  let (p1,p2) = (podPosition (pss!!i1),podPosition (pss!!i2))
      (v1,v2) = (podSpeed    (pss!!i1),podSpeed    (pss!!i2))
      -- move p1 to the origin
      
      
collide2Points :: PodID -> PodID -> [PodState] -> [PodState]
collide2Points ::
  
-- | Decay speed of a pod due to friction   
speedDecay :: PodState -> PodState
speedDecay ps{podSpeed = speed} = ps{podSpeed = 0.85 V.`scalarMul` speed}



 
