module CSB.Player.IO
    ( InitInput(..)
    , TurnInput(..)
    , TurnOutput(..)
    , PodMovement(..)
    , Thrust(..)
    )
where

import           Data.Vec2
import           CSB.Game.State

type InitInput = GameSpec

data TurnInput = TurnInput { selfPod1Info     :: PodInfo
                           , selfPod2Info     :: PodInfo
                           , opponentPod1Info :: PodInfo
                           , opponentPod2Info :: PodInfo
                           } deriving (Show, Read)

data TurnOutput = TurnOutput { selfPod1Movement :: PodMovement
                             , selfPod2Movement :: PodMovement
                             } deriving (Show, Read)

