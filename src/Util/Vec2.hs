{-# LANGUAGE BangPatterns#-}

module Data.Vec2
    ( Vec2(..)
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
    ,unitVec
    )
where

data Vec2 = Vec2 Double Double deriving (Show, Read, Eq)

instance Num Vec2 where
    Vec2 x1 y1 + Vec2 x2 y2 = Vec2 (x1 + x2) (y1 + y2)
    Vec2 x1 y1 - Vec2 x2 y2 = Vec2 (x1 - x2) (y1 - y2)
    (*) = error "multiplication on Vec2 is not supported"
    negate (Vec2 x y) = Vec2 (-x) (-y)
    abs    = error "abs on Vec2 is not supported"
    signum = error "signum on Vec2 is not supported"
    fromInteger 0 = Vec2 0 0
    fromInteger _ = error "fromInteger x for x /= 0 on Vec2 is not supported"

scalarMul :: Double -> Vec2 -> Vec2
scalarMul !c (Vec2 !x !y) = Vec2 (c * x) (c * y)

scalarDiv :: Vec2 -> Double -> Vec2
scalarDiv (Vec2 !x !y) c = Vec2 (x / c) (y / c)

dot :: Vec2 -> Vec2 -> Double
Vec2 !x1 !y1 `dot` Vec2 !x2 !y2 = x1 * x2 + y1 * y2

norm :: Vec2 -> Double
norm v = sqrt $ dot v v

dist :: Vec2 -> Vec2 -> Double
dist v1 v2 = norm $ v1 - v2

arg :: Vec2 -> Double
arg (Vec2 !x !y) = atan2 y x

-- | project v1 onto v2
proj :: Vec2 -> Vec2 -> Vec2
proj v1 v2 = ((v1 `dot` v2)/(v2 `dot` v2)) `scalarMul` v2

angleBetween :: Vec2 -> Vec2 -> Double
angleBetween from to = signum (rotate90 from `dot` to)
    * acos ((from `dot` to) / (norm from * norm to))

rotate :: Double -> Vec2 -> Vec2
rotate theta (Vec2 !x !y) =
    Vec2 (cos theta * x - sin theta * y) (sin theta * x + cos theta * y)

unitVect :: Double -> Vec2
unitVect theta  = Vec2 (cos theta) (sin theta) 

rotate90 :: Vec2 -> Vec2
rotate90 (Vec2 x y) = Vec2 (-y) x

reflect :: Vec2 -> Vec2 -> Vec2
reflect mirror v = (2 `scalarMul` projection) - v
  where
    mirrorUnit = mirror `scalarDiv` norm mirror
    projection = (mirrorUnit `dot` v) `scalarMul` mirrorUnit

isZero :: Vec2 -> Bool
isZero (Vec2 x y) = x == 0 && y == 0
