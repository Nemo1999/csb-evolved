{-# LANGUAGE ScopedTypeVariables #-}
module GameRule
  (
  GameSpec(..)
  ,initPodStates
  ,randomGameSpecIO
  ,runGame
  ,maxSimTurn
  ,GameHistory(..)
  ,gameEnd
  )
where


import qualified Util as U
import Data.Vec2(Vec2)
import qualified Data.Vec2 as V
import GameSim
import Player
import System.Random
import Control.Monad.Fix
maxSimTurn = 2000 :: Int

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
  in   return $ map (\pos->emptyPodState{podPosition=pos,podNextCheckPoints=tail podckpts}) podPos




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


gameEnd :: GameHistory -> Bool
gameEnd = any (\p->podNextCheckPoints p == []) . head  


playerDrivePod :: (PlayerIO p1, PlayerIO p2) => p1 -> p2 -> [PodState] -> IO (p1 , p2 ,[PodState]) 
playerDrivePod p1 p2 g = do                                                              
    let p1In = PlayerIn (take 2 g) (drop 2 g)                                            
    let p2In = PlayerIn (drop 2 g) (take 2 g)                                            
    ( [p1Out1,p1Out2] , p1') <- playerRunIO p1 p1In                                       
    ( [p2Out1,p2Out2] , p2') <- playerRunIO p2 p2In                                       
    let g' = [(g!!0){podMovement=p1Out1},(g!!1){podMovement=p1Out2},                     
          (g!!2){podMovement=p2Out1},(g!!3){podMovement=p2Out2}]                         
    return (p1', p2',g')                                                                 

-- Game history , the time goes from right to left in the list 
type GameHistory = [[PodState]]

runGame ::
  forall player1  player2. (PlayerIO player1 , PlayerIO player2) =>
  (player1 , player2)
  -> GameSpec 
  -> (GameHistory -> Bool)  -- Stop Rule 
  -> IO GameHistory        
  
runGame (p1,p2) gameSpec stopRule = do
    p1Init <-playerInitIO p1 :: IO player1
    p2Init <-playerInitIO p2 :: IO player2
    g0  <-  initPodStates gameSpec
    
    (p1',p2',g1) <- playerDrivePod p1Init p2Init g0
    let g1' = map (thrustPod.rotatePod) g1
    
    simulate p1' p2' [g1'] stopRule 



simulate ::
  forall player1 player2. (PlayerIO player1 , PlayerIO player2) =>
  player1
  -> player2
  -> [[PodState]] -- Initial State
  -> (GameHistory -> Bool)  -- stop rule 
  -> IO GameHistory
simulate p1 p2 gss@(g:gs) stopRule = do
  let g0 = map speedDecay $ movePods 1 g
  (p1',p2',g1) <- playerDrivePod p1 p2 g0 
  let g2 = map (thrustPod.rotatePod) g1
  if (length gss > maxSimTurn) || stopRule gss
    then return gss
    else simulate p1' p2' (g2:gss) stopRule
      
      
