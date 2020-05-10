{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGES PatternGuards #-}
module Main

where
import Data.Vec2
import Util
import GameRule
import Interact
import Player
import System.TimeIt
import GameRule
import GameSim
import Player.Instances
import Player.GA
import Player.GAM
import Data.Maybe
import System.Environment
import System.Exit

p3 = WrapIO (ElementaryPlayer ())
p2 = GASimple
p1 = defaultGAMeta

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

type PlayerID = Int 
data PlayerMode = Default PlayerID | IOMode | Executable FilePath 

data GameConfig = GameConfig{
  player1 :: PlayerMode
  player2 :: PlayerMode
  showAnimatetion :: Bool,
  saveGameHistory :: Maybe FilePath
  playGameHistory :: Maybe FilePath
                        }

defaultGameConfig = GameConfig (Default 1) (Default 2) True Nothing Nothing

parsePlayerMode :: String -> IO PlayerMode
parsePlayerMode playerArg
  |all isDigit playerArg
  = Default (read playerArg)
  |"io"<-playerArg
  = IOMode
  | otherwise
  = Executable palyerArg 

parseArgs :: [String] -> GameConfig -> IO GameConfig
parseArgs args config
  | []<-args
  = return $ config
  | "-p1":playerArg:rest <-args
  = parseArgs rest config{player1=parsePlayerMode playerArg}
  | "-p2":playerArg:rest <-args
  = parseArgs rest config{player2=parsePlayerMode playerArg}
  | "-noAnimation":rest <- args
  = parseArgs rest config{showAnimatetion=False}
  | "-saveGame":savePath:rest <- args
  = parseArgs rest config{saveGameHistory=Just savePath}
  | "-playFile":gameHistPath:rest <- args
  = parseArgs rest config{playGameHistory=Just gameHistPath}
  | otherwise
  = do printUsage
       exitWith $ ExitFailure 1

printUsage :: IO()
printUsage = putStrLn $ unlines
  ["Usage: csb-evolved [flages] "
  ,"  -p1/-p2    <player arg>      specify players"
  ,"  -noAnimation                 don't show animation"
  ,"  -saveGame  <save path>       save game log"
  ,"  -playFile  <open path>       show animation of saved game"
  ,""
  ,"-----------------------------------------"
  ,"<player arg>:"
  ,"  <ID ::Int>                   default players"
  ,"  <path :: FilePath>           executable file"
  ,"  \"io\":: String                play interactively with standard in/out"]


main :: IO ()
main = do
  args <- getArgs
  gConfig <- parseArgs args defaultGameConfig
  
