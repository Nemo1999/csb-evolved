{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGES PatternGuards #-}
module Main

where
import Data.Vec2
import Util
import GameRule
import Interact
import Player
import qualified  Interact
import System.TimeIt
import GameRule
import GameSim
import Player(WrapIO(..))
import Player.Instances
import Player.GA
import Player.GAM
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Data.Vec2(Vec2(..),scalarMul)
import Data.List.Index
import Data.Maybe
import System.Environment

p1 = WrapIO (ElementaryPlayer ())
p2 = GASimple
p3 = defaultGAMeta

testGameSim :: IO GameHistory
testGameSim = do
  gsp  <- randomGameSpecIO
  ghis <- runGame (p1,p1) gsp gameEnd
  return ghis

testAnimate :: Double ->  IO()
testAnimate turnPerSec = do
  gsp <- randomGameSpecIO
  ghis <- runGame (p2,p3) gsp gameEnd
  gameAnimateIO turnPerSec  gsp ghis

data PlayerMode = Compiled | IOMode | Executable FilePath FilePath

data GameMode = GameMode{
  playerMode :: PlayerMode ,
  animate :: Bool,
  saveGameHistory :: Maybe FilePath  
                        }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> testAnimate 5 
