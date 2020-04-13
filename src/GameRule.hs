{-# LANGUAGE ScopedTypeVariables #-}
module GameRule
  (
  GameSpec(..)
  ,initPodStates
  ,randomGameSpecIO
  ,runGame
  ,maxSimTurn
  ,GameHistory(..)
  )
where


import qualified Util as U
import Data.Vec2(Vec2)
import qualified Data.Vec2 as V
import GameSim
import Player
import System.Random
import Control.Monad.Fix
maxSimTurn = 300 :: Int

-- | Informaiton about the GameSetting
data GameSpec = GameSpec { gameSLaps :: Int
                         , gameSCheckpoints :: [Vec2] } deriving (Show, Read)
-- | generate random gamespec



emptyMovement = PodMovement V.zeroVec $ Normal 0
emptyPodState = PodState V.zeroVec V.zeroVec Nothing True Nothing emptyMovement []

initPodStates :: GameSpec -> IO([PodState])
initPodStates (GameSpec laps ckpts) =
  let podckpts =concat $ replicate laps ckpts
      (ckpt0,ckpt1) = (head ckpts , head $ tail ckpts)
      perp = V.rotate90 (ckpt1-ckpt0)
      shift = (410 / V.norm perp) `V.scalarMul` perp
      podPos = [ckpt0+shift,
                ckpt0 + 3`V.scalarMul`shift,
                ckpt0-shift,
                ckpt0 - 3`V.scalarMul`shift]
  --in U.randomPerm $  map (\pos->emptyPodState{podPosition=pos,podNextCheckPoints=podckpts}) podPos
  in   return $ map (\pos->emptyPodState{podPosition=pos,podNextCheckPoints=podckpts}) podPos



randomGameSpecIO :: IO GameSpec
randomGameSpecIO = do
    nCkpts <- randomRIO (3, 8)::IO Int
    ckpts  <- genCkpts nCkpts
    return $ GameSpec 3 ckpts
  where
    genCkpts 0 = return []
    genCkpts n = do
        ckpts <- genCkpts (n - 1)
        ckpt  <- fix $ \rec -> do
            ckpt <- V.randomVec V.zeroVec U.gameWorldSize

            if hasOverlap ckpt ckpts then rec else return ckpt

        return $ ckpt : ckpts

    hasOverlap ckpt =
        any (\ckpt' -> V.dist ckpt ckpt' < 2 * U.checkPointRadius) 


gameEnd :: [PodState] -> Bool
gameEnd = any (\p->podNextCheckPoints p == []) 

-- Game history , the time goes from right to left in the list 
type GameHistory = [[PodState]]

runGame ::forall player1  player2. (PlayerIO player1 , PlayerIO player2) => (player1 , player2) -> GameSpec-> IO GameHistory
runGame (p1,p2) gameSpec = do
    p1 <-playerInitIO :: IO player1
    p2 <-playerInitIO :: IO player2
    g0 <-  initPodStates gameSpec
    simulate p1 p2 (pure [g0]) 
      where simulate :: player1 -> player2 -> IO GameHistory -> IO GameHistory 
            simulate p1 p2 gssIO = 
              do
                  gss <- gssIO
                  let g:gRest = gss
                  
                  -- if it is first turn , then playerDrive -> rotatePod -> thrustPod  , else do nothing
                  (p1Now,p2Now,gTurned) <- if length gRest == 0 then playerDrive p1 p2 g else return (p1,p2,g)
                  let gNow = if length gRest==0 then map (thrustPod.rotatePod) gTurned else gTurned

                  -- if any pod finish laps , simulation ends
                  if gameEnd gNow then gssIO else
                    -- if simulation turns > maxSimTurn  , simulation ends
                    if length gss >= maxSimTurn then gssIO else
                      do
                        -- MovePods , reduceSpeed
                        let gNew = map speedDecay $ movePods 1 gNow
                        -- drivePods
                        (p1' ,p2',g') <- playerDrive p1Now p2Now gNew
                        -- turnPod , thrustPod
                        let gResult = map (thrustPod.rotatePod) g'
                        -- recurse to next turn
                        simulate p1' p2' (pure (gResult:gss))
                        
                  where playerDrive :: player1 -> player2 -> [PodState] -> IO (player1 , player2,[PodState])
                        playerDrive p1 p2 g = do
                            let p1In = PlayerIn (take 2 g) (drop 2 g)                          
                            let p2In = PlayerIn (drop 2 g) (take 2 g)                          
                            ( [p1Out1,p1Out2] , p1') <- playerRunIO p1In p1                          
                            ( [p2Out1,p2Out2] , p2') <- playerRunIO p2In p2                          
                            let g' = [(g!!0){podMovement=p1Out1},(g!!1){podMovement=p1Out2},
                                  (g!!2){podMovement=p2Out1},(g!!3){podMovement=p2Out2}]
                            return (p1', p2',g')
