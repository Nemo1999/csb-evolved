module Player.Process
    ( Process
    , newProcess
    )
where

import           Debug.Trace
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.IO
import           System.Process

import           Data.Vec2
import           GameSim
import           Player
import           Util

data Process = UninitialisedProcess String | InitialisedProcess [Vec2] Handle Handle ProcessHandle | CleanedUpProcess

newProcess :: String -> Process
newProcess = UninitialisedProcess

instance PlayerIO Process where
    playerInitIO = return

    playerRunIO (UninitialisedProcess cmd) playerIn = do
        (Just stdinHdl, Just stdoutHdl, _, procHdl) <- createProcess
            CreateProcess { cmdspec            = ShellCommand cmd
                          , cwd                = Nothing
                          , env                = Nothing
                          , std_in             = CreatePipe
                          , std_out            = CreatePipe
                          , std_err            = Inherit
                          , close_fds          = False
                          , create_group       = False
                          , delegate_ctlc      = False
                          , detach_console     = False
                          , create_new_console = False
                          , new_session        = False
                          , child_group        = Nothing
                          , child_user         = Nothing
                          , use_process_jobs   = False
                          }

        let checkpointCount =
                getMinRepLen $ podNextCheckPoints (head (selfPod playerIn))
            checkpoints =
                take checkpointCount
                    . drop (checkpointCount - 1)
                    $ podNextCheckPoints (head (selfPod playerIn))
            laps =
                length (podNextCheckPoints (head (selfPod playerIn)))
                    `div` checkpointCount

        hPrint stdinHdl laps
        hPrint stdinHdl checkpointCount
        forM_ checkpoints $ \(Vec2 x y) ->
            hPutStrLn stdinHdl (show (round x) ++ " " ++ show (round y))

        playerRunIO
            (InitialisedProcess checkpoints stdinHdl stdoutHdl procHdl)
            playerIn

    playerRunIO p@(InitialisedProcess checkpoints stdinHdl stdoutHdl procHdl) playerIn
        | any (null . podNextCheckPoints) (selfPod playerIn)
            || any (null . podNextCheckPoints) (oppoPod playerIn)
        = cleanupProcess (Just stdinHdl, Just stdoutHdl, Nothing, procHdl)
            >> return (replicate 2 dummyOut, CleanedUpProcess)
        | otherwise
        = do
            forM_ (selfPod playerIn) putPodState
            forM_ (oppoPod playerIn) putPodState
            hFlush stdinHdl

            outs <- replicateM 2 getPodMovement

            return (outs, p)
      where
        putPodState PodState { podPosition = Vec2 x y, podSpeed = Vec2 vx vy, podAngle = angle, podNextCheckPoints = (checkpoint : _) }
            = hPutStrLn stdinHdl
                $ unwords
                . map show
                $ [ round x
                  , round y
                  , round vx
                  , round vy
                  , maybe 0 (round . radToDeg) angle
                  , fromJust (elemIndex checkpoint checkpoints)
                  ]

        getPodMovement = do
            [xStr, yStr, thr] <- words <$> hGetLine stdoutHdl

            return PodMovement
                { podTarget = Vec2 (read xStr) (read yStr)
                , podThrust = case thr of
                                  "SHIELD" -> Shield
                                  "BOOST"  -> Boost
                                  _        -> Normal (read thr)
                }

    playerRunIO CleanedUpProcess _ =
        return (replicate 2 dummyOut, CleanedUpProcess)

    playerFinalizeIO (UninitialisedProcess _) = return ()
    playerFinalizeIO (InitialisedProcess _ stdinHdl stdoutHdl procHdl) =
        cleanupProcess (Just stdinHdl, Just stdoutHdl, Nothing, procHdl)
    playerFinalizeIO CleanedUpProcess = return ()

dummyOut :: PodMovement
dummyOut = PodMovement { podTarget = Vec2 0 0, podThrust = Normal 0 }
