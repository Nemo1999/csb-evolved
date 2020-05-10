{-#LANGUAGE BangPatterns#-}
{-# OPTIONS_GHC -O2  #-}
module Interact
  (
    gameAnimateIO  
  )
where
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
import qualified Util as U

-- Every thing is scaled by 0.1 times on the screen 
scaleFactor :: Float
scaleFactor = 0.1

-- Time = Double defined in GameSim
type World  = Time

vec2Point :: Vec2 -> Point
vec2Point (Vec2 !x !y) = (realToFrac x  * scaleFactor , realToFrac y * scaleFactor)


makePicture :: (String,String) -> GameSpec -> [PodState] -> Picture
makePicture (name1,name2) (GameSpec _ ckpts) ps =
  let
    [s11,s12,s21,s22] = map (measurePodScore. getPodScore) ps
    p1Win = (max s11 s12 >  max s21 s22)
    legends = izipWith3 drawLegends [blue,red] [name1,name2] [p1Win,not p1Win]
    pods = (zipWith drawPod [blue,blue,red,red]  ps)
    ckpts = imap drawCheckPoint ckpts 
    picture = Pictures $ legends ++ pods ++ ckpts
    (shiftX,shiftY)  = vec2Point ((-0.5) `scalarMul` U.gameWorldSize)
  in Translate shiftX shiftY picture
    where
          drawCheckPoint :: Int -> Vec2 -> Picture
          drawCheckPoint n  pos =
            let (x,y) = vec2Point pos
            in 
              Color green $ 
                 Translate x y  $
                 Pictures [Scale 0.2 0.2 $ Text $ show n , ThickCircle (realToFrac U.checkPointRadius*scaleFactor) 5]
          drawPod :: Color -> PodState -> Picture
          drawPod c pod =
            let dir@[(x,y),(tX,tY)] = [vec2Point $ podPosition pod , vec2Point $ podTarget  $ podMovement pod ] 
                ang = fromMaybe 0 $ podAngle pod
                podR = realToFrac U.podForceFieldRadius * scaleFactor
                circleBody =  circleSolid podR
                sqrt05 = sqrt 0.5 
                dirTriangle=  Rotate (realToFrac $ U.radToDeg (-ang)) $ Polygon [(podR,0),((-sqrt05)*podR ,(sqrt05)*podR),((-sqrt05)*podR,(-sqrt05)*podR)] 
            in
                 Pictures $ [ (Color c $ Line dir)   ,
                              Translate x y $ Pictures [Color c $ circleBody , Color yellow $ dirTriangle] ]
          drawLegends :: Int -> Color -> String -> Bool -> Picture
          drawLegends index c name isWinner =
            let (posX,posY) = (fromIntegral 20,fromIntegral $ 50+index*30)
                square = Translate posX posY $ Color c $ rectangleSolid 25 25
                text = Translate (posX+35) posY $ Color c $ Text name
                winnerSign = Translate (posX + 100) posY $ if isWinner then Color orange $ Text "<< 1'st >>"
                                                                       else Color (dim orange)$ Text "<< 2'nd >>"
            in  Pictures $ [square,text,winnerSign]




gameAnimateIO :: (String,String) -> Double -> GameSpec -> GameHistory -> IO() 
gameAnimateIO (name1,name2) -> turnPerSec  gameSpec gs =
  let
    window = InWindow "pod-race simulation" (1600, 900) (0,0)
    initWorld = 0 :: World
    draw :: World -> IO Picture
    draw time = do
      let ps = gs !! max 0 ((length gs - 1 ) - fromInteger (floor time))
      let psNow = movePods (time - fromIntegral (floor time)) ps
      return $ makePicture (name1,name2) gameSpec psNow --should use psNow instead
    eventHandler _ = pure
    updateWorld :: Float -> World -> IO World
    updateWorld time w = do
      return (w + (realToFrac time) * turnPerSec)
  in playIO window black 10 initWorld draw eventHandler updateWorld 
    

-- Testing





  



defaultGS1 = GameSpec 3 [Vec2 10545.0 6005.0,Vec2 3559.0 5204.0,Vec2 13574.0 7618.0,Vec2 12476.0 1344.0]
initPodPos1 = [Vec2 10488.0 6502.0,Vec2 10602.0 5508.0,Vec2 10374.0 7495.0,Vec2 10716.0 4515.0]

defaultGS2 = GameSpec 3 [Vec2 7285.0 6638.0,Vec2 5397.0 2810.0,Vec2 10336.0 3368.0,Vec2 11222.0 5395.0]
initPodPos2 = [Vec2 6837.0 6859.0,Vec2 7733.0 6417.0,Vec2 5940.0 7302.0,Vec2 8630.0 5974.0]

defaultGS3  = GameSpec 3 [Vec2 5398.0 2821.0,Vec2 10295.0 3372.0,Vec2 11182.0 5424.0,Vec2 7248.0 6684.0]
initPodPos3 = [Vec2 5454.0 2324.0,Vec2 5342.0 3318.0,Vec2 5566.0 1330.0,Vec2 5230.0 4312.0]

defaultGS4 = GameSpec 3[Vec2 5999.0 5364.0,Vec2 11271.0 2827.0,Vec2 7479.0 6944.0]
initPodPos4 = [Vec2 5782.0 4913.0,Vec2 6216.0 5815.0,Vec2 5349.0 4012.0,Vec2 6649.0 6716.0]

defaultGS5 = GameSpec 3 [Vec2 6015.0 5383.0,Vec2 11272.0 2830.0,Vec2 7498.0 6936.0]
initPodPos5 = [Vec2 5797.0 4933.0,Vec2 6233.0 5833.0,Vec2 5360.0 4034.0,Vec2 6670.0 6732.0]

defaultGS6 = GameSpec 3 [Vec2 11325.0 2820.0,Vec2 7530.0 6922.0,Vec2 6022.0 5368.0]
initPodPos6 = [Vec2 11692.0 3160.0,Vec2 10958.0 2480.0,Vec2 12426.0 3839.0,Vec2 10224.0 1801.0]

defaultGS7 = GameSpec 3 [Vec2 13906.0 1191.0,Vec2 10230.0 4907.0,Vec2 6105.0 2218.0,Vec2 3030.0 5182.0,Vec2 6287.0 7788.0,Vec2 14097.0 7733.0]

defaultGS8 = GameSpec 3[Vec2 8698.0 7466.0,Vec2 7215.0 2140.0,Vec2 3575.0 5281.0,Vec2 13830.0 5104.0,Vec2 10660.0 2270.0]

defaultGameSpecs = [defaultGS1,defaultGS2,defaultGS3,defaultGS4,defaultGS5,defaultGS6,defaultGS7,defaultGS8] 


