module CSB.Player
    ( Player(..)
    )
where

import           CSB.Player.IO

class Player p where
    playerInit :: InitInput -> p
    playerRun   :: TurnInput -> p -> (TurnOutput, p)
class PlayerIO p where
    playerInitIO :: InitInput -> IO p
    playerRunIO   :: TurnInput -> p -> IO (TurnOutput , p) 
