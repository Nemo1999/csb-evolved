{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE PatternGuards #-}
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
import Data.Char

(p1,name1) = (defaultGAMeta,"Boss1")
(p2,name2) = (GASimple , "Boss2")
(p3,name3) = (WrapIO (ElementaryPlayer ()),"Boss3")

testGameSim :: IO GameHistory
testGameSim = do
  gsp  <- randomGameSpecIO
  ghis <- runGame (p1,p1) gsp gameEnd
  return ghis

testAnimate :: Double ->  IO()
testAnimate turnPerSec = do
  gsp <- randomGameSpecIO
  ghis <- runGame (p3,p1) gsp gameEnd
  gameAnimateIO ("player1","player2") turnPerSec  gsp ghis

type PlayerID = Int 
data PlayerMode = Default PlayerID String  | IOMode String | Executable FilePath String  deriving (Show,Read)

data GameConfig = GameConfig{
  player1 :: PlayerMode,
  player2 :: PlayerMode,
  showAnimatetion :: Bool,
  saveGameHistory :: Maybe FilePath,
  playGameHistory :: Maybe FilePath
                        } deriving (Show ,Read)

defaultGameConfig = GameConfig (Default 3 name3) (Default 3 name3) True Nothing Nothing

data SaveType = SaveType (String ,String) GameSpec GameHistory deriving (Show,Read)

parsePlayerMode :: String -> String -> PlayerMode
parsePlayerMode playerArg name
  |all isDigit playerArg
  = Default (read playerArg) name
  |"io"<-playerArg
  = IOMode name
  | otherwise
  = Executable playerArg name 

parseArgs :: [String] -> GameConfig -> IO GameConfig
parseArgs args config
  | []<-args
  = return $ config
  | "-p1":playerArg:playerName:rest <-args
  = parseArgs rest config{player1=parsePlayerMode playerArg playerName}
  | "-p2":playerArg:playerName:rest <-args
  = parseArgs rest config{player2=parsePlayerMode playerArg playerName}
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
  ,"  -p1/-p2    <PlayerMode> <Name>    specify players"
  ,"  -noAnimation                      don't show animation"
  ,"  -saveGame  <Save Path>            save game log"
  ,"  -playFile  <Open Path>            show animation of saved game"
  ,""
  ,"-----------------------------------------"
  ,"<player arg>:"
  ,"  <ID ::Int>                   default players"
  ,"  <Path :: FilePath>           executable file"
  ,"  \"io\"                     play interactively with standard in/out"]


main :: IO ()
main = do
  args <- getArgs
  config <- parseArgs args defaultGameConfig
  case playGameHistory config of
    Just path -> do
      SaveType (name1,name2) gameSpec gameHist <-read<$>readFile path
      gameAnimateIO (name1,name2) 4.5 gameSpec gameHist
      exitWith ExitSuccess
    Nothing   -> return ()
  
  testAnimate 4.5
  
  
      


