{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
module Player.GA
(
  GASimple(..),
  getPodScore,
  measurePodScore
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
  
-------- Parameters
geneLength = 6 :: Int
popSize = 16 :: Int 
pCross = 0.8 :: Double
pMutate = 0.8 :: Double
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

crossover ::Int ->  Gene -> Gene -> IO (Gene,Gene)
crossover geneLength g1 g2 = do
  n <- randomIO :: IO Double
  k <- randomIO :: IO Double
  if n>pCross then return (g1,g2) else do
    crossPoint <- randomRIO (1,geneLength-1)
    let g1' = take crossPoint g1 ++ drop crossPoint g2
    let g2' = take crossPoint g2 ++ drop crossPoint g1
    return (midValues k g1' g2',midValues k g2' g1')
    where
      midValues k  = zipWith (\(ang1,th1) (ang2,th2)->(ang1*k + ang2*(1-k),th1)) 

------------- Random Population

randomPop  :: Int -> Int -> IO Population
randomPop geneLength popSize =
  let randomPair = (,) <$> randomGene geneLength <*> randomGene geneLength
  in  sequence $ replicate popSize randomPair

randomGene :: Int -> IO Gene
randomGene geneLength = sequence $ replicate geneLength randomStep

randomStep :: IO Step
randomStep = do
  i <- randomRIO ((-1),1) ::IO Double
  let delAngle = 18*i
  n        <- randomRIO (0,49) :: IO Double                
  let thrust = case n of                                
        _ | 0<=n  && n<10 -> Normal 0                   
          | 10<=n && n<20 -> Normal 80                 
          | 20<=n && n<30 -> Normal 100                 
          | 30<=n && n<40 -> Boost                      
          | 35<=n && n<50 -> Shield                     
  return (delAngle,thrust)

--------------- Mutation

mutate :: Int -> Gene -> IO Gene
mutate geneLength g = do
  n <- randomIO :: IO Double
  if n>pMutate then return g else do 
    mutatePoint <- randomRIO (0,geneLength-1)
    let (oldAng, oldThrust) = g!!mutatePoint  
    (newAng,newThrust) <- randomStep
    angOrThrust <- randomIO :: IO Bool
    let newStep = if angOrThrust then (newAng,oldThrust) else (oldAng,newThrust)
    return (take mutatePoint g ++ [newStep] ++ drop (mutatePoint+1) g)  

------------- Fitness 

fitness   :: [PodState]-> (Gene,Gene) -> Double 
fitness initialState genePair =
  let simResult = simTeam geneLength (makeDriver genePair , defaultOpponent) initialState
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

simTeam :: Int -> (Driver,Driver) -> [PodState] -> [PodState]
simTeam 0 (player ,opponent) psNow = psNow
simTeam n (player ,opponent) psNow =
  let ps' = player (geneLength-n) (take 2 psNow) ++ opponent (geneLength - n) (drop 2 psNow)
  in  simTeam (n-1) (player,opponent) $ gameSimTurns 1 ps'

-- | Calculate the fitness of a (Gene,Gene) for (pod1,pod2)
measureTeam :: [PodState] -> Double 
measureTeam  [p1,p2,o1,o2] =
 let pMax = max (getPodScore p1) (getPodScore p2)
     oMax = max (getPodScore o1) (getPodScore o2)
 in  (measureTeamScore teamscoreMeasureSelfOppoRate $ TeamScore pMax oMax)  
     - if (podThrust (podMovement p1) == Boost) then boostPenalty else 0
     - if (podThrust (podMovement p2) == Boost) then boostPenalty else 0
     + (0.8*(measurePodScore $ min (getPodScore p1)(getPodScore p2)))

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

data GASimple = GASimple
instance PlayerIO GASimple where
  playerInitIO _ = return GASimple
  playerRunIO _ PlayerIn{selfPod=[p1,p2],oppoPod=[o1,o2]} = do
    t0 <- getCPUTime
    let defaultSeed = (defaultGene p1 ,defaultGene p2)

    --popRand <- randomPop geneLength (popSize)
    --let pop0 = sortOn (negate.fitness [p1,p2,o1,o2]) $  popRand
    
    popRand <- randomPop geneLength (popSize-2)
    let pop0 = sortOn (negate.fitness [p1,p2,o1,o2]) $  defaultSeed:defaultSeed:popRand
    
    popFinal <- evolve t0 [p1,p2,o1,o2] pop0 0
    return (decodeGene (p1,p2) $  head popFinal , GASimple)
    where
      decodeGene :: (PodState,PodState) -> (Gene,Gene) -> [PodMovement]
      decodeGene (ps1,ps2) (g1,g2) = [decodeStep ps1 $ head g1 , decodeStep ps2 $ head g2]
      evolve :: Integer -> [PodState]-> Population -> Int -> IO Population 
      evolve t0 ps g0 gCnt= do
        t1 <- getCPUTime
        if (t1-t0)>maxTime then return $
          trace (show gCnt )-- ++" "++show (head g0))
                                                $ g0 else do
          mg1 <-(nextGen geneLength ps g0) -- timeout 10 ....
          case Just mg1 of
            Just  g1 -> evolve t0 ps g1 (gCnt+1)
            Nothing  -> return g0
      
      
-- | Decode a Gene as PodMovement
decodeStep :: PodState -> Step -> PodMovement
decodeStep PodState{podPosition=pos,podAngle = podAng,podNextCheckPoints = ckpts} (ang,thrust)
  | Just angBase <- podAng = PodMovement (pos + (5000 `scalarMul` unitVec (degToRad ang+angBase) )) thrust
  | Nothing      <- podAng = PodMovement (head ckpts) thrust

  
nextGen :: Int -> [PodState] -> Population -> IO Population
nextGen geneLength  podStates ps = do
  let [p1,p2] = take 2 ps
  matingPool <- select popSize ps 
  childrens  <- crossoverAndMutate geneLength matingPool
  return $ take popSize $ sortOn (negate.fitness podStates) (p1:p2:childrens)
    where
      select :: Int -> Population -> IO Population
      select n ps = randomPerm $ take n $ cycle $ take (n `div` 2) ps  
      crossoverAndMutate :: Int -> Population -> IO Population -- length population should be even 
      crossoverAndMutate geneLength ((g11,g12):(g21,g22):gs) = do
            (g11', g21') <- crossover geneLength g11 g21
            (g12', g22') <- crossover geneLength g12 g22 
            [g11'',g12'', g21'' , g22''] <- sequence $ map (mutate geneLength) [g11', g12', g21', g22']
            gs'' <- crossoverAndMutate geneLength gs
            return ((g11'',g12''):(g21'',g22''):gs'')
      crossoverAndMutate geneLength [] =return  []

-- Default Gene
encode :: PodState -> PodMovement -> Step
encode PodState{podPosition=pos,podAngle=ang} PodMovement{podTarget=target,podThrust = thrust}
  = let tarAng = radToDeg $ arg $ (target-pos)
        turnAng= clamp (-18) (normalizeDeg (tarAng - (radToDeg.fromJust) ang)) (18)
    in (turnAng,thrust)

defaultGene :: PodState-> Gene
defaultGene ps =
  let 
      iterFunc = (\x ->  (speedDecay $driftPod 1$ thrustPod $ rotatePod $ defaultDriver x ))
      defaultDriverHistory = iterate iterFunc ps
  in map (\ps -> encode ps (podMovement ps)) $ take geneLength $ tail defaultDriverHistory 
         



