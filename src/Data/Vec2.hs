{-# LANGUAGE BangPatterns#-}
{-# OPTIONS_GHC -O2 #-}
module Data.Vec2
    ( Vec2(..)
    , elementWise
    , elementWise2
    , scalarMul
    , scalarDiv
    , dot
    , norm
    , dist
    , arg
    , rotate
    , rotate90
    , reflect
    , isZero
    , unitVec
    ,proj
    ,zeroVec
    , randomVec
    ,roundVec
    )
where
import System.Random



data Vec2 = Vec2 {-# UNPACK #-} !(Double)
                 {-# UNPACK #-} !(Double)  deriving (Show, Read, Eq)

instance Num Vec2 where
    (+)    = elementWise2 (+)
    (-)    = elementWise2 (-)
    (*)    = elementWise2 (*)
    negate = elementWise negate
    abs    = elementWise abs
    signum = elementWise signum
    fromInteger x = Vec2 (fromInteger x) (fromInteger x)

elementWise :: (Double -> Double) -> Vec2 -> Vec2
elementWise f (Vec2 x y) = Vec2 (f x) (f y)

elementWise2 :: (Double -> Double -> Double) -> Vec2 -> Vec2 -> Vec2
elementWise2 f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 `f` x2) (y1 `f` y2)

scalarMul :: Double -> Vec2 -> Vec2
scalarMul !c (Vec2 !x !y) = Vec2 (c * x) (c * y)

scalarDiv :: Vec2 -> Double -> Vec2
scalarDiv (Vec2 !x !y) !c = Vec2 (x / c) (y / c)

dot :: Vec2 -> Vec2 -> Double
(Vec2 !x1 !y1) `dot` (Vec2 !x2 !y2) = x1 * x2 + y1 * y2

norm :: Vec2 -> Double
norm !v = sqrt $ dot v v

dist :: Vec2 -> Vec2 -> Double
dist v1@(Vec2 !x1 !y1) v2@(Vec2 !x2 !y2) = norm $ v1 - v2

arg :: Vec2 -> Double
arg (Vec2 !x !y) = atan2 y x

-- | project v1 onto v2
proj :: Vec2 -> Vec2 -> Vec2
proj v1@(Vec2 !x1 !y1) v2@(Vec2 !x2 !y2) = ((v1 `dot` v2)/(v2 `dot` v2)) `scalarMul` v2

angleBetween :: Vec2 -> Vec2 -> Double
angleBetween from to = signum (rotate90 from `dot` to)
    * acos ((from `dot` to) / (norm from * norm to))

rotate :: Double -> Vec2 -> Vec2
rotate theta (Vec2 !x !y) =
    Vec2 (cos theta * x - sin theta * y) (sin theta * x + cos theta * y)

unitVec :: Double -> Vec2
unitVec theta  = Vec2 (cos theta) (sin theta) 

rotate90 :: Vec2 -> Vec2
rotate90 (Vec2 !x !y) = Vec2 (-y) x

reflect :: Vec2 -> Vec2 -> Vec2
reflect mirror !v = (2 `scalarMul` projection) - v
  where
    mirrorUnit = mirror `scalarDiv` norm mirror
    projection = (mirrorUnit `dot` v) `scalarMul` mirrorUnit

isZero :: Vec2 -> Bool
isZero (Vec2 !x !y) = x == 0 && y == 0

-- |zero Vector
zeroVec = Vec2 0 0

-- | random point in the rectangle area defined by p1 and p2
randomVec :: Vec2 -> Vec2 -> IO Vec2
randomVec p1@(Vec2 !x1 !y1) p2@(Vec2 !x2 !y2)
 = let (minX,maxX)=(min x1 x2 , max x1 x2)
       (minY,maxY)=(min y1 y2 , max y1 y2)
   in  Vec2 <$> (randomRIO (minX,maxX)) <*> (randomRIO (minY,maxY))

roundVec  :: Vec2 -> Vec2
roundVec (Vec2 !x !y) = Vec2 (fromIntegral $ round x) (fromIntegral $ round y)  
  
