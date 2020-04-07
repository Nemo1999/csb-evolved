module GameRule (

                )
where

import qualified Util as U
import Data.Vec2(Vec2)
import qualified Data.Vec2 as V
import GameSim
import GameRule.Player
import System.Random

-- | Informaiton about the GameSetting
data GameSpec = GameSpec { gameSLaps :: Int
                         , gameSCheckpoints :: [Vec2] } deriving (Show, Read)
-- | generate random gamespec
randomGameSpecIO :: IO GameSpec
randomGameSpecIO = 
  let ckptIO = replicate <$> (randomRIO (3,8)) <*> (V.randomVec V.zeroVec U.gameWorldSize) 
  in  GameSpec 3 <$> ckptIO

emptyMovement = PodMovement V.zeroVec $ Normal 0
emptyPodState = PodState V.zeroVec V.zeroVec Nothing True emptyMovement []

initPodStates GameSec -> IO ([PodState])
initPodStates (GameSpec laps ckpts) =
  let podckpts = replicate laps ckpts
      (ckpt0,ckpt1) = (head ckpts , head $ head ckpts)
      perp = V.rotate90 (ckpt1-ckpt0)
      shift = (410 / norm perp) `V.scalarMul` perp
      podPos = [ckpt0+shift,ckpt0-shift,
                ckpt0 + 3`V.scalarMul`shift,
                ckpt0 - 3`V.scalarMul`shift]
  in U.randomPerm $ map (\pos->emptyPodState{podPosition=pos,podNextCheckPoints=posckpts}) podPos



