{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Player
    (
      PlayerIn(..)
      ,PlayerOut(..)
      ,Player(playerInit,playerRun)
      ,PlayerIO(playerInitIO,playerRunIO)
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


-- | every Player p can be used as PlayerIO p     
instance (Player p) => PlayerIO p where  
  playerInitIO = return playerInit
  playerRunIO  pin p = return $ playerRun pin p
    
-- Testing ------------------


instance Player Int  where
  playerInit = 0
  playerRun  _ p = ([],p+1)

instance PlayerIO Bool where
  playerInitIO = pure False
  playerRunIO _ p = return $ ([],not p )
