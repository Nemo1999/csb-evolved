{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE  ExistentialQuantification #-}
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
import Player.Process
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
  ghis <- runGame (p2,p1) gsp gameEnd
  gameAnimateIO ("player1","player2") turnPerSec  gsp ghis

data PlayerIOObj = forall a. (PlayerIO a) => PlayerIOObj a

instance PlayerIO PlayerIOObj where
  playerInitIO (PlayerIOObj p) =
    fmap PlayerIOObj $ playerInitIO p
  playerRunIO (PlayerIOObj p) pIn =
    fmap (\(o,p)->(o,PlayerIOObj p)) (playerRunIO p pIn)

type PlayerID = Int 
data PlayerMode = Default PlayerID String  | IOMode String | Executable FilePath String  deriving (Show,Read)
makePlayer :: PlayerMode -> (PlayerIOObj,String)
makePlayer (Default n name) = case n of
  1 -> (PlayerIOObj p1,name)
  2 -> (PlayerIOObj p2,name)
  3 -> (PlayerIOObj p3,name)
makePlayer (IOMode name) = error "IOMode not supported" 
makePlayer (Executable filepath name) = (PlayerIOObj $ newProcess filepath, name)

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
  ,"  \"io\"                         play interactively with standard in/out"]


main :: IO ()
main = do
  args <- getArgs
  config <- parseArgs args defaultGameConfig
  case playGameHistory config of
    Just path -> do
      SaveType (name1,name2) gameSpec gameHist <-read<$>readFile path
      gameAnimateIO (name1,name2) 4.5 gameSpec gameHist
      exitWith ExitSuccess
    Nothing   -> do
      let (p1,name1) = makePlayer $ player1 config
      let (p2,name2) = makePlayer $ player2 config
      gsp <- randomGameSpecIO
      ghis <- runGame (p1,p2) gsp gameEnd
      
      if isJust $ saveGameHistory config then
        writeFile (fromJust $ saveGameHistory config) (show $ SaveType (name1,name2) gsp ghis)
        else pure ()
      if showAnimatetion config then
        gameAnimateIO (name1,name2) 6 gsp ghis
        else print $ winner (head ghis)
  
      


