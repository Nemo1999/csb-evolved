module GameRule (

                )
where

import Util
import Data.Vec2
import CSB.Param


data Gamelog p1 p2 =  (GameState p1 p2) TurnOutput  
-- Informaiton about the GameSetting
data GameSpec = GameSpec { gameSLaps :: Int
                         , gameSCheckpoints :: [Vec2] } deriving (Show, Read)

-- Information needed by player
data PodInfo = PodInfo { podPosition         :: Vec2
                       , podSpeed            :: Vec2
                       , podAngle            :: Double
                       , podNextCheckPointId :: Int
                       } deriving (Show, Read)
data GameRuleState = {podLap::Int
             n

                     }
