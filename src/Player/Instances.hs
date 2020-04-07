module Player.Instances
    ( ElementaryPlayer(..)
    )
where

import           Data.Vec2
import           GameSim
import           Player

newtype ElementaryPlayer = ElementaryPlayer () deriving (Show)

instance Player ElementaryPlayer where
    playerInit = ElementaryPlayer ()
    playerRun PlayerIn { selfPod = ss } _ =
        (map straightToTarget ss, ElementaryPlayer ())
      where
        straightToTarget PodState { podNextCheckPoints = ckpts } = PodMovement
            { podTarget = case ckpts of
                              []         -> Vec2 0 0
                              target : _ -> target
            , podThrust = Normal 100
            }
