module Player
    (
      PlayerIn(..)
      ,PlayerOut(..)
      ,Player(playerInit,playerRun)
      ,PlayerIO(playerInitIO,playerRunIO)
      ,WrapIO(..)
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

newtype WrapIO p = WrapIO p

-- | every Player p can be used as PlayerIO p     
instance (Player p) => PlayerIO (WrapIO p) where  
  playerInitIO = return $ WrapIO playerInit
  playerRunIO pin (WrapIO p) =
    let (pout, p') = playerRun pin p in return (pout, WrapIO p')
    
-- Testing ------------------


instance Player Int  where
  playerInit = 0
  playerRun  _ p = ([],p+1)

instance PlayerIO Bool where
  playerInitIO = pure False
  playerRunIO _ p = return $ ([],not p )
