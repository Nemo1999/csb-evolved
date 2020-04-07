{-#LANGUAGE BangPatterns#-}
module Interact
  (
    gamePlayIO
  )
where
import GameRule
import GameSim
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Data.Vec2(Vec2(..))
import Data.List.Index
import qualified Util as U

-- Every thing is scaled by 0.1 times on the screen 
scaleFactor :: Float
scaleFactor = 0.1

type World = Double

vec2Point :: Vec2 -> Point
vec2Point (Vec2 !x !y) = (realToFrac x  * scaleFactor , realToFrac y * scaleFactor)



makePicture :: GameSpec -> [PodState] -> Picture
makePicture (GameSpec _ ckpts) ps =
  Pictures $ imap drawCheckPoint (vec2Point<$>ckpts) ++
             (zipWith drawPod [blue,blue,red,red] $ map (vec2Point.podPosition) ps)  
    where drawCheckPoint :: Int -> Point -> Picture
          drawCheckPoint n (x,y) = 
            Color green $
               Translate x y  $
               Pictures [Text $ show n , ThickCircle (realToFrac U.checkPointRadius*scaleFactor) 5] 
          drawPod :: Color -> Point -> Picture
          drawPod c (x,y) =
            Color c $
              Translate x y $
              circleSolid   $ (realToFrac U.podForceFieldRadius*scaleFactor)
          

gamePlayIO :: GameSpec -> GameHistory -> IO() 
gamePlayIO gameSpec gs =
  let
    window = InWindow "pod-race simulation" (1600, 900) (0,0)
    initWorld = 0 :: World
    draw :: World -> IO Picture
    draw k = do
      let ps = gs !! max 0 (maxSimTurn - fromInteger (ceiling k))
      return $ makePicture gameSpec ps
    eventHandler _ = pure
    updateWorld :: Float -> World -> IO World
    updateWorld time w = do
      return (w + realToFrac time)
  in playIO window black 3 initWorld draw eventHandler updateWorld 
    
