module GameRule.Player
    (
      PlayerIn(..)
      ,PlayerOut(..)
      ,Player
      ,PlayerIO
    )
where

import GameSim(PodState,PodMovement)




data PlayerIn = PlayerIn {selfPod ::[PodState]
                         ,oppoPod ::[PodState]} 

type PlayerOut = [PodMovement]

class Player p where
    playerInit :: p
    playerRun  :: PlayerIn -> p -> (PlayerOut, p)
class PlayerIO p where
    playerInitIO :: IO p
    playerRunIO   :: PlayerIn -> p -> IO (PlayerOut , p) 
