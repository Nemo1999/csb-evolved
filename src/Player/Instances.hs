module Player.Instances
    ( DefaultPlayer(..)
    , ElementaryPlayer(..)
    , 
    )
where

import           Data.Vec2
import           GameSim
import           Player

-- newtype IdlePlayer = IdlePlayer () deriving (Show)



newtype DefaultPlayer = DefaultPlayer () deriving (Show)

instance Player DefaultPlayer where
    playerInit = id
    playerRun _ _ = (replicate 2 movementPerPod, DefaultPlayer ())
      where
        movementPerPod =
            PodMovement { podTarget = Vec2 8000 4500, podThrust = Normal 100 }

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
