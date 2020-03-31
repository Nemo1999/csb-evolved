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
import Data.Vec2(Vec2)
import qualified Data.Vec2 as V
import qualified Util as U



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

type Angle = Double


-- | simulate Game Phisic for one turn
gameSimTurn :: [PodState] -> [PodState]
gameSimTurn pss = map speedDecay $ movePod $ map updatePod pss


-- | Update the PodState ( Except for podPostion ) according to current PodMovement 
updatePod :: PodState -> PodState 
updatePod ps@(PodState position speed angle boostAvail shieldState (Movement target thrust))
   = let
         shieldState' = shieldNextState (thrust == Shield) shieldState
         idle = isJust shieldState'         
         accMag = if idle then 0 else
           case thrust of
             Normal n -> n 
             Boost    -> if boostAvail then U.boostAccel else 100
         accel  = accMag V.`scalarMul` rotate angle' $ Vec2 1 0
         speed'= if idle then speed else speed + accel
         deltaAng = (U.normalizeAngle (V.arg (target - position) - angle))
         angle' = angle + U.clamp (- U.maxTurnAngle) deltaAng U.maxTurnAngle
         boostAvail' = boostAvail && (thrust \= Boost)
      in
         ps{podSpeed=speed',podAngle=angle',podBoostAvail=boostAvail',podShieldState = shieldState'}

-- | Move Pod according to PodState (Including updating new Speed after Collision)
movePod   :: [PodState] -> [Podstate]
movePod pss = 

speedDecay :: PodState -> PodState
speedDecay ps{podSpeed = speed} = ps{podSpeed = 0.85 V.`scalarMul` speed}



 
