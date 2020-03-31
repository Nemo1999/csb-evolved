module CSB.Player.Instances
    ( ElementaryPlayer(..)
    )
where

import           CSB.IO
import           CSB.Player

newtype ElementaryPlayer = ElementaryPlayer InitInput deriving (Show)

instance Player ElementaryPlayer where
    initState = ElementaryPlayer
    runTurn TurnInput { selfPod1Info = PodInfo { podNextCheckPointId = i }, selfPod2Info = PodInfo { podNextCheckPointId = j } } (ElementaryPlayer initInput)
        = ( TurnOutput
              { selfPod1Movement = PodMovement { podTarget = checkpoints !! i
                                               , podThrust = Normal 100
                                               }
              , selfPod2Movement = PodMovement { podTarget = checkpoints !! j
                                               , podThrust = Normal 100
                                               }
              }
          , ElementaryPlayer initInput
          )
        where checkpoints = gameCheckpoints initInput
