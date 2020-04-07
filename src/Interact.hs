module Interact
  (gamePlay
  ,gameWatch
  )
where
import GameRule
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Data.Vec2(Vec2(..))
import Data.List.Index
import qualified Util as U

-- Every thing is scaled by 0.1 times on the screen 
scale :: Float
scale 0.1

type World = Double

vec2Point :: Vec2 -> Point
vec2Point Vec2 !x !y = (realToFrac x  * scale , realToFrac y * scale)



makePicture :: GameSpec -> [PodStates] -> Picture
makePicture (GameSpec _ ckpts) ps =
  Pictures $ imap drawCheckPoint (vec2Point<$>ckpts) ++
             zipWith drawPod [blue,blue,red,red] $ map (vec2Point.podPosition) ps  
    where drawCheckPoint :: Int -> Point -> Picture
          drawCheckPoint n (x,y) = 
            Color green $
               Translate x y  $
               Pictures [Text show n , ThickCircle (realToFrac U.checkPointRadius*scale) 5] 
          drawPod :: Color -> Point -> Picture
          drawPod c (x,y) =
            Color c $
              Translate x y $
              circleSolid   $ realToFrac (U.podForceFieldRadius*scale)
          

gamePlayIO :: GameSpec -> [[PodStates]] -> IO() 
gamePlayIO gameSpec gs =
  let
    initWorld = 0 :: World
    draw :: World -> IO Picture
    draw k = do
      let ps = gs !! max 0 (maxSimTurn - fromInteger (ceiling k))
      return $ makePicture gameSpec ps
    eventHandler _ = pure
  in playIO black 3 initWorld draw eventHandler
    
