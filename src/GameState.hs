module CSB.Game.State
    ( GameSpec(..)
    , GameState(..)
    , PodState(..)
    , PodInfo(..)
    , PodStates(..)
    )
where


import           CSB.Param
import           Data.Vec2
import           Util


-- Informaiton about the GameSetting
data GameSpec = GameSpec { gameSLaps :: Int
                         , gameSCheckpoints :: [Vec2] } deriving (Show, Read)

-- All Informations about the Game
data GameState p1 p2 = GameState { gamespec     :: GameSpec
                                 , podStates    :: [PodState]
                                 , player1      :: p1
                                 , player2      :: p2
                                 , firstRound   :: Bool
                                 } deriving (Show, Read)

-- the list containing 4 PodState [ p11 , p12 , p21 , p22 ]
type PodStates =  [PodState] 


-- Information needed by player
data PodInfo = PodInfo { podPosition         :: Vec2
                       , podSpeed            :: Vec2
                       , podAngle            :: Double
                       , podNextCheckPointId :: Int
                       } deriving (Show, Read)

-- Informaiton needed by simulator
data PodState = PodState { info                 :: PodInfo
                         , podSLaps             :: Int
                         , podSBoostAvail       :: Bool
                         } deriving (Show, Read)



--runGame :: (Player p1, Player p2) => GameSpec -> [GameState p1 p2]
--runGame spec = iterate (runGameTurn spec) (newGame spec)



-- -- TODO: Properly calculate initial position and angle.
-- newGame :: (Player p1, Player p2) => GameSpec -> GameState p1 p2
-- newGame spec = GameState
--     { podStates = PodStates [ player1Pod1State = initPodState (Vec2 8000 3000) 0
--                             , player1Pod2State = initPodState (Vec2 8000 4000) 0
--                             , player2Pod1State = initPodState (Vec2 8000 5000) 0
--                             , player2Pod2State = initPodState (Vec2 8000 6000) 0
--                             ]
--     , playerStates = PlayerStates { player1State = initState initInput
--                                   , player2State = initState initInput
--                                   }
--     , firstRound = True
--     }
--     where initInput = gameSpecToInitInput spec

-- initPodState :: Vec2 -> Double -> PodState
-- initPodState position angle = PodState { podSPosition         = position
--                                        , podSSpeed            = Vec2 0 0
--                                        , podSAngle            = angle
--                                        , podSNextCheckPointId = 1
--                                        , podSLaps             = 0
--                                        , podSBoostAvail       = True
--                                        }

-- -- TODO: Implement end-game logic.
-- runGameTurn
--     :: (Player p1, Player p2) => GameSpec -> GameState p1 p2 -> GameState p1 p2
-- runGameTurn spec GameState { podStates = s, playerStates = PlayerStates { player1State = p1, player2State = p2 }, firstRound = fr }
--     = GameState
--         { podStates    = s'
--         , playerStates = PlayerStates { player1State = p1', player2State = p2' }


--         , firstRound   = False
--         }
--   where
--     (i1, i2 ) = podStatesToTurnInput s
--     (o1, p1') = runTurn i1 p1
--     (o2, p2') = runTurn i2 p2
--     s'        = sim spec fr s o1 o2

-- sim :: GameSpec -> Bool -> PodStates -> TurnOutput -> TurnOutput -> PodStates
-- sim spec fr PodStates { player1Pod1State = s11, player1Pod2State = s12, player2Pod1State = s21, player2Pod2State = s22 } TurnOutput { selfPod1Movement = o11, selfPod2Movement = o12 } TurnOutput { selfPod1Movement = o21, selfPod2Movement = o22 }
--     = podStatesAfterMovement
--   where
--     podStatesAfterMovement = PodStates
--         { player1Pod1State = simPod spec fr s11 o11
--         , player1Pod2State = simPod spec fr s12 o12
--         , player2Pod1State = simPod spec fr s21 o21
--         , player2Pod2State = simPod spec fr s22 o22
--         }
--     podStatesAfterCollisionDetection = simCollision podStatesAfterMovement

-- simPod :: GameSpec -> Bool -> PodState -> PodMovement -> PodState
-- simPod GameSpec { gameSCheckpoints = checkpoints } fr PodState { podSPosition = r, podSSpeed = v, podSAngle = t, podSNextCheckPointId = i, podSLaps = l, podSBoostAvail = boostAvail } PodMovement { podTarget = tgt, podThrust = thr }
--     = PodState { podSPosition         = r'
--                , podSSpeed            = v'
--                , podSAngle            = t'
--                , podSNextCheckPointId = i'
--                , podSLaps             = l'
--                , podSBoostAvail       = boostAvail'
--                }
--   where
--     dtr               = normalizeAngle (arg (tgt - r) - t)
--     dt = if fr then dtr else clamp (-maxTurnAngle) dtr maxTurnAngle
--     t'                = dt + t

--     (am, boostAvail') = case thr of
--         Normal t -> (fromIntegral t, boostAvail)
--         Shield   -> (0, boostAvail)
--         Boost    -> if boostAvail then (boostAccel, False) else (0, False)
--     a            = am `scalarMul` rotate t' (Vec2 1 0)

--     v''          = v + a
--     r'           = r + v''
--     v'           = 0.85 `scalarMul` v''

--     inCheckpoint = dist r' (checkpoints !! i) < checkpointRadius
--     i'           = if inCheckpoint then succ i `mod` length checkpoints else i
--     l'           = if inCheckpoint && i == 0 then succ l else l

-- -- TODO: Implement shield.
-- simCollision :: PodStates -> PodStates
-- simCollision PodStates { player1Pod1State = s11, player1Pod2State = s12, player2Pod1State = s21, player2Pod2State = s22 }
--     = PodStates { player1Pod1State = s11'
--                 , player1Pod2State = s12'
--                 , player2Pod1State = s21'
--                 , player2Pod2State = s22'
--                 }
--   where
--     [s11', s12', s21', s22'] = simCollision' [s11, s12, s21, s22]

--     simCollision' []       = []
--     simCollision' (s : ss) = h : simCollision' t
--       where
--         (h, t) = foldr f (s, []) ss
--         f s1 (s2, ss) =
--             let (s1', s2') = simCollisionTwoPods s1 s2 in (s1', s2' : ss)

--     simCollisionTwoPods s1 s2 =
--         (s1 { podSSpeed = v1' }, s2 { podSSpeed = v2' })
--       where
--         r1 = podSPosition s1
--         r2 = podSPosition s2
--         v1 = podSSpeed s1
--         v2 = podSSpeed s2

--         (v1', v2') =
--             if dist r1 r2 < 2 * podForceFieldRadius then (v2, v1) else (v1, v2)
