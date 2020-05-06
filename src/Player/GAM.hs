{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
module Player.GAM
(
  GAMeta(..),
  defaultGAMeta
)
where
import Player
import Data.Vec2
import GameSim
-- import GameRule
import Util
import Data.List
import Data.Maybe

import System.Random
import System.Timeout
import System.CPUTime
import Debug.Trace

{- --- Just for Look up ~~
data GAMeta = GAMeta{seed::Bool,
                     angRes::Bool
                     geneLength::Int,
                     popSize::Int,
                     pMutate::Double,
                     pCross::Double,
                     select::Int -> Population -> IO Population,
                     crossover ::Int->Double->Gene->Gene->IO (Gene,Gene)
                     mutation  ::Bool -> Int->Double->Gene-> IO Gene
                    }

-}
-------- Parameters
defaultGAMeta = GAMeta{seed=False, --- do we put artifitial seed in the initial Population?
                       angRes=True, --- do we divide random generated angle range (-18,18) into 3 or 5 part  
                       geneLength=6,
                       popSize=8, 
                       pMutate=1,
                       pCross=1, 
                       select=defaultSelect, 
                       crossover=defaultCross, 
                       mutation = defaultMutate
                      }
                       
boostPenalty = 10000 :: Double
podScoreCkptWeight = 16000 :: Int
teamscoreMeasureSelfOppoRate = 1
maxTime = 71000000000 :: Integer -- maximum time before returning the final answer 

--------- Types
-- | In each step the pod turns between [-18,+18] degrees 
type DeltaAngle = Angle
-- | Step is an action to take in one step
type Step = ( DeltaAngle , Thrust )
-- | Genotype of the realTime GA is a list of action to take in the following steps 
type  Gene = [Step]
-- | A population is a list of (Gene for Pod1 , Gene for Pod2 ) sorted by fitness (the head is the best individual)  
type Population = [(Gene,Gene)]

-- | PodScore is used to calculate fitness 
-- | The higher the score , the closer the Destination
-- | Int is the length of the up comming checkpoints
-- | double is the distance to the next checkpoint
data PodScore =  PodScore (Int,Double) deriving Eq
instance Ord PodScore where
  compare ps1 ps2 =
    compare  (measurePodScore  ps1)
             $ measurePodScore ps2

-- | The Score of a simulation result of a pair of Genes
-- | Contains the PodScore of player's best Pod and opponent's best Pod  
data  TeamScore = TeamScore{selfBest::PodScore , oppoBest::PodScore} deriving Eq
instance Ord TeamScore where
  compare ts1 ts2
    = compare  (measureTeamScore 1 ts1) $ measureTeamScore 1 ts2

-------------CrossOver 

defaultCross ::Int->Double ->  Gene -> Gene -> IO (Gene,Gene)
defaultCross  geneLength pCross  g1 g2 = do
  n <- randomIO :: IO Double
  k <- randomIO :: IO Double
  if n>pCross then return (g1,g2) else do
    crossPoint <- randomRIO (1,geneLength-1)
    let g1' = take crossPoint g1 ++ drop crossPoint g2
    let g2' = take crossPoint g2 ++ drop crossPoint g1
    return (midValues k g1' g2',midValues k g2' g1')
    where
      midValues k  = zipWith (\(ang1,th1) (ang2,th2)->(ang1*k + ang2*(1-k),th1))

-------------Selection
defaultSelect :: Int -> Population -> IO Population
defaultSelect popSize ps = randomPerm $ take popSize $ cycle $ take (popSize `div` 2) ps  

------------- Random Population

randomPop  :: Bool -> Int -> Int -> IO Population
randomPop angRes geneLength popSize =
  let randomPair = (,) <$> randomGene angRes geneLength <*> randomGene angRes geneLength
  in  sequence $ replicate popSize randomPair

randomGene :: Bool -> Int -> IO Gene
randomGene angRes geneLength = sequence $ replicate geneLength $ randomStep angRes

randomStep :: Bool -> IO Step
randomStep angleRes = do
  let res = if angleRes then 1 else 2
  i <- randomRIO ((-res),res) ::IO Int
  let delAngle = (18/fromIntegral res)*(fromIntegral i)
  n        <- randomRIO (0,49) :: IO Double                
  let thrust = case n of                                
        _ | 0<=n  && n<10 -> Normal 0                   
          | 10<=n && n<20 -> Normal 50                 
          | 20<=n && n<30 -> Normal 100                 
          | 30<=n && n<40 -> Boost                      
          | 35<=n && n<50 -> Shield                     
  return (delAngle,thrust)

--------------- Mutation

defaultMutate ::Bool -> Int-> Double -> Gene -> IO Gene
defaultMutate angRes  geneLength pMutate  g = do
  n <- randomIO :: IO Double
  if n>pMutate then return g else do 
    mutatePoint <- randomRIO (0,geneLength-1)
    let (oldAng, oldThrust) = g!!mutatePoint  
    (newAng,newThrust) <- randomStep angRes 
    angOrThrust <- randomIO :: IO Bool
    let newStep = if angOrThrust then (newAng,oldThrust) else (oldAng,newThrust)
    return (take mutatePoint g ++ [newStep] ++ drop (mutatePoint+1) g)  

------------- Fitness 

fitness   :: Int -> [PodState]-> (Gene,Gene) -> Double 
fitness geneL initialState genePair =
  let simResult = simTeam geneL geneL(makeDriver genePair , defaultOpponent) initialState
  in  measureTeam simResult

-- | take step number and a list of 2 PodStates and update the "PodMovement" of the two pods
type Driver = Int -> [PodState]->[PodState]
defaultOpponent :: Driver  
defaultOpponent _  = map defaultDriver 

-- | default Driver , just move toward nextckpt
defaultDriver ::  PodState -> PodState
defaultDriver ps@PodState{podPosition = pos,podAngle = ang,podNextCheckPoints=cs} =
  let thValue =  if isJust ang then if abs (normalizeDeg $ (radToDeg $  arg (head cs - pos)) - (radToDeg.fromJust) ang )>90 then 0  else 100 else 100
  in if length cs > 0 then ps{podMovement = PodMovement (head cs) (Normal thValue)} else ps 
-- | make Driver from given Gene
makeDriver :: (Gene,Gene) -> Driver
makeDriver (g1,g2) n [p1,p2] =
  [p1{podMovement = decodeStep p1 (g1!!n)},p2{podMovement = decodeStep p2 (g2!!n)}]

simTeam :: Int -> Int -> (Driver,Driver) -> [PodState] -> [PodState]
simTeam _ 0 (player ,opponent) psNow = psNow
simTeam geneLen n (player ,opponent) psNow =
  let ps' = player (geneLen - n) (take 2 psNow) ++ opponent (geneLen - n) (drop 2 psNow)
  in  simTeam geneLen (n-1) (player,opponent) $ gameSimTurns 1 ps'

-- | Calculate the fitness of a (Gene,Gene) for (pod1,pod2)
measureTeam :: [PodState] -> Double 
measureTeam  [p1,p2,o1,o2] =
 let pMax = max (getPodScore p1) (getPodScore p2)
     oMax = max (getPodScore o1) (getPodScore o2)
 in  (measureTeamScore teamscoreMeasureSelfOppoRate $ TeamScore pMax oMax)  
     - if (podThrust (podMovement p1) == Boost) then boostPenalty else 0
     - if (podThrust (podMovement p2) == Boost) then boostPenalty else 0
     + ((min (measurePodScore $ getPodScore p1)(measurePodScore $ getPodScore p2)))

-- | turn TeamScore into Double for compare
-- | the higher the score , the better the team
measureTeamScore :: Double -> TeamScore -> Double
measureTeamScore selfOppoRate TeamScore{selfBest=s,oppoBest = o} =
    selfOppoRate * measurePodScore s -  measurePodScore o  
 
-- | turn PodScore into Double for compare
measurePodScore ::  PodScore -> Double
measurePodScore (PodScore (i,d))=
  ( negate $ fromIntegral ( podScoreCkptWeight * i)) - d
-- | Evalutating the score of a single  PodState
getPodScore :: PodState -> PodScore
getPodScore PodState{podPosition=pos,podAngle = (Just ang),podNextCheckPoints = ckpts} =
  let len = length ckpts
      dist = if len > 0 then norm ((pos+(250`scalarMul`unitVec ang)) -head ckpts) else 0
  in PodScore (len ,dist)


-------------Define Player

-- | A simple GA player (no configuring parameter for now)

data GAMeta = GAMeta{seed::Bool,
                     angRes::Bool,
                     geneLength::Int,
                     popSize::Int,
                     pMutate::Double,
                     pCross::Double,
                     select::Int -> Population -> IO Population,
                     crossover ::Int->Double->Gene->Gene->IO (Gene,Gene),
                     mutation  ::Bool -> Int->Double->Gene-> IO Gene
                    }

instance PlayerIO GAMeta where
  playerInitIO  = return 
  playerRunIO player@(GAMeta seed angRes  geneLength popSize _ _ _ _ _) PlayerIn{selfPod=[p1,p2],oppoPod=[o1,o2]} = do
    t0 <- getCPUTime
    let defaultSeed = (defaultGene geneLength p1 ,defaultGene geneLength p2)
        
    let randSize = if seed then (popSize-2) else popSize
    popRand <- randomPop angRes  geneLength randSize
    let pop0 = sortOn (negate.fitness geneLength [p1,p2,o1,o2]) $ if seed then defaultSeed:defaultSeed:popRand else popRand
    popFinal <- evolve player t0 [p1,p2,o1,o2] pop0 0
    return (decodeGene (p1,p2) $  head popFinal ,player)
    where
      decodeGene :: (PodState,PodState) -> (Gene,Gene) -> [PodMovement]
      decodeGene (ps1,ps2) (g1,g2) = [decodeStep ps1 $ head g1 , decodeStep ps2 $ head g2]
      evolve :: GAMeta -> Integer -> [PodState]-> Population -> Int -> IO Population 
      evolve player t0 ps g0 gCnt= do
        t1 <- getCPUTime
        if (t1-t0)>maxTime then return $
          trace (show gCnt )-- ++" "++show (head g0))
                                                $ g0 else do
          mg1 <-(nextGen player  ps g0) -- timeout 10 ....
          case Just mg1 of
            Just  g1 -> evolve player t0 ps g1 (gCnt+1)
            Nothing  -> return g0
      
      
-- | Decode a Gene as PodMovement
decodeStep :: PodState -> Step -> PodMovement
decodeStep PodState{podPosition=pos,podAngle = podAng,podNextCheckPoints = ckpts} (ang,thrust)
  | Just angBase <- podAng = PodMovement (pos + (5000 `scalarMul` unitVec (degToRad ang+angBase) )) thrust
  | Nothing      <- podAng = PodMovement (head ckpts) thrust

  
nextGen :: GAMeta -> [PodState] -> Population -> IO Population
nextGen player@(GAMeta seed angRes geneLength popSize pMutate pCross select crossover mutate) podStates ps = do
  let [p1,p2] = take 2 ps
  matingPool <- select popSize ps 
  childrens  <- crossoverAndMutate  matingPool
  return $ take popSize $ sortOn (negate.fitness geneLength podStates) (p1:p2:childrens)
    where
      crossoverAndMutate :: Population -> IO Population -- length population should be even 
      crossoverAndMutate  ((g11,g12):(g21,g22):gs) = do
            (g11', g21') <- crossover geneLength pCross g11 g21
            (g12', g22') <- crossover geneLength pMutate g12 g22 
            [g11'',g12'', g21'' , g22''] <- sequence $ map (mutate angRes geneLength pMutate) [g11', g12', g21', g22']
            gs'' <- crossoverAndMutate gs
            return ((g11'',g12''):(g21'',g22''):gs'')
      crossoverAndMutate  [] =return  []

-- Default Gene
encode :: PodState -> PodMovement -> Step
encode PodState{podPosition=pos,podAngle=ang} PodMovement{podTarget=target,podThrust = thrust}
  = let tarAng = radToDeg $ arg $ (target-pos)
        turnAng= clamp (-18) (normalizeDeg (tarAng - (radToDeg.fromJust) ang)) (18)
    in (turnAng,thrust)

defaultGene :: Int -> PodState-> Gene
defaultGene geneL  ps =
  let 
      iterFunc = (\x ->  (speedDecay $driftPod 1$ thrustPod $ rotatePod $ defaultDriver x ))
      defaultDriverHistory = iterate iterFunc ps
  in map (\ps -> encode ps (podMovement ps)) $ take geneL $ tail defaultDriverHistory 
         


