module Main where

import           CSB.Game
import           CSB.Player.Instances
import           Data.Vec2

main :: IO ()
main = do
    let dummySpec = GameSpec
            { gameSLaps        = 3
            , gameSCheckpoints = [ Vec2 1000  1000
                                 , Vec2 1000  15000
                                 , Vec2 15000 1000
                                 , Vec2 15000 15000
                                 ]
            }
        gameStates =
            runGame dummySpec :: [GameState ElementaryPlayer ElementaryPlayer]

    print $ take 100 gameStates
