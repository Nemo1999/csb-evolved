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
randomGameSpec :: IO GameSpec
randomGameSpec = 
  let ckptIO = replicate <$> (randomRIO (3,8)) <*> (V.randomVec V.zeroVec U.gameWorldSize) 
  in  GameSpec 3 <$> ckptIO
