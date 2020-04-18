{-# LANGUAGE BangPatterns #-}
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
    playerInit :: p -> p  
    playerRun  :: p -> PlayerIn -> (PlayerOut, p)
class PlayerIO p where
    playerInitIO :: p -> IO p
    playerRunIO  :: p -> PlayerIn -> IO (PlayerOut , p)

newtype WrapIO p = WrapIO p

-- | every Player p can be used as PlayerIO p     
instance (Player p) => PlayerIO (WrapIO p) where  
  playerInitIO (WrapIO !p)  = return  $ WrapIO $ playerInit p
  playerRunIO  (WrapIO !p)  !pin =
    let (!pout, !p') = playerRun p pin  in return (pout, WrapIO p')
    
-- Testing ------------------


instance Player Int  where
  playerInit = id
  playerRun p  _  = ([],p+1)

instance PlayerIO Bool where
  playerInitIO = return 
  playerRunIO  p _  = return $ ([],not p )
