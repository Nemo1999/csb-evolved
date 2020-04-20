module Player.Instances
    ( DefaultPlayer(..)
    , ElementaryPlayer(..)
    , ElementarySearchPlayer(..)
    )
where

import           Debug.Trace
import           Data.List
import           Data.Maybe
import           Data.Vec2
import           GameSim
import           Player
import           Util

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

newtype ElementarySearchPlayer = ElementarySearchPlayer () deriving (Show)

instance Player ElementarySearchPlayer where
  playerInit = id

  playerRun _ PlayerIn { selfPod = ss } =
    (map decision ss, ElementarySearchPlayer ())
   where
    nRounds     = 6
    nCandidates = 100

    decision :: PodState -> PodMovement
    decision podState =
      espPodMovementToPodMovement podState $ searchOptimalMovement podState

    searchOptimalMovement :: PodState -> ESPPodMovement
    searchOptimalMovement podState = search nRounds [initSearchState]
     where
      search 0 (SearchState { searchStateFirstMovement = Just m } : _) = m
      search n searchStates = search (pred n) searchStates'
       where
        searchStates' =
          take nCandidates
            . sortOn searchStateMetric
            . concatMap deriveSearchState
            $ searchStates

      initSearchState = SearchState { searchStatePodState           = podState
                                    , searchStateFirstMovement      = Nothing
                                    , searchStateNChecrkPointPassed = 0
                                    }

      movements =
        [ ESPPodMovement { podMovementTurnAngle = (-pi) / 10
                         , podMovementThrust    = Normal 0
                         }
        , ESPPodMovement { podMovementTurnAngle = 0
                         , podMovementThrust    = Normal 0
                         }
        , ESPPodMovement { podMovementTurnAngle = pi / 10
                         , podMovementThrust    = Normal 0
                         }
        , ESPPodMovement { podMovementTurnAngle = (-pi) / 10
                         , podMovementThrust    = Normal 100
                         }
        , ESPPodMovement { podMovementTurnAngle = 0
                         , podMovementThrust    = Normal 100
                         }
        , ESPPodMovement { podMovementTurnAngle = pi / 10
                         , podMovementThrust    = Normal 100
                         }
        ]
      passedCheckPoint PodState { podNextCheckPoints = (p : _) } PodState { podNextCheckPoints = (q : _) }
        = p /= q
      deriveSearchState SearchState { searchStatePodState = podState, searchStateFirstMovement = m, searchStateNChecrkPointPassed = n }
        = map f movements
       where
        f movement =
          let podState' = simulate podState movement
          in
            SearchState
              { searchStatePodState           = podState'
              , searchStateFirstMovement      = initialize movement m
              , searchStateNChecrkPointPassed =
                if passedCheckPoint podState podState' then succ n else n
              }

      searchStateMetric SearchState { searchStatePodState = PodState { podPosition = r, podNextCheckPoints = (p : _) }, searchStateNChecrkPointPassed = n }
        = (-n, dist r p)

    -- Simulation ---------------------------------------------------------

    espPodMovementToPodMovement :: PodState -> ESPPodMovement -> PodMovement
    espPodMovementToPodMovement PodState { podPosition = r, podAngle = theta } ESPPodMovement { podMovementTurnAngle = deltatheta, podMovementThrust = thr }
      = PodMovement { podTarget = target, podThrust = thr }
     where
      target =
        1000 `scalarMul` rotate (fromMaybe 0 theta + deltatheta) (Vec2 1 0) + r

    simulate :: PodState -> ESPPodMovement -> PodState
    simulate podState ESPPodMovement { podMovementTurnAngle = deltatheta, podMovementThrust = thr }
      = podState { podPosition        = r'
                 , podSpeed           = v'
                 , podAngle           = Just theta'
                 , podNextCheckPoints = ps'
                 }
     where
      PodState { podPosition = r, podSpeed = v, podAngle = theta, podNextCheckPoints = (p : ps) }
        = podState

      -- Rotation.
      theta' = normalizeAngle $ fromMaybe 0 theta + deltatheta

      -- Acceleration.
      am     = case thr of
        Normal x -> fromIntegral x
        Boost    -> 650
        Shield   -> 0
      a            = am `scalarMul` rotate theta' (Vec2 1 0)
      v''          = v + a

      -- Movement.
      r'           = r + v''

      -- Friction.
      v'           = 0.85 `scalarMul` v''

      -- Checkpoint.
      inCheckpoint = dist r' p < 600
      ps'          = if inCheckpoint then ps else p : ps

    initialize :: a -> Maybe a -> Maybe a
    initialize x m = case m of
      Nothing -> Just x
      Just _  -> m

data SearchState = SearchState { searchStatePodState           :: PodState
                               , searchStateFirstMovement      :: Maybe ESPPodMovement
                               , searchStateNChecrkPointPassed :: Int
                               } deriving (Show)

data ESPPodMovement = ESPPodMovement { podMovementTurnAngle :: Double
                                     , podMovementThrust    :: Thrust
                                     } deriving (Show)
