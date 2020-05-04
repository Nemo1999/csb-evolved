{-# OPTIONS_GHC -O2  #-}
{-# LANGUAGE BangPatterns #-}

module Player.GA
(
  
)
where
import Warrier.Weapons

import qualified Data.Vector as V
import System.Random
import System.Timeout
import System.CPUTime

-- Gene class
class Chromosone a where
  crossover :: a -> a -> IO a
  muation   :: a -> IO a
  fitness   :: [PodState]-> a -> Double 

---- Declare the player Instance 
-- | In each step the pod turns between [-18,+18] degrees 
type DeltaAngle = Angle
-- | Step is an action to take in one step
type Step = ( DeltaAngle , Thrust )
-- | Genotype of the realTime GA is a list of action to take in the following steps 
type  Gene = [Step]
-- | A population is a list of (Gene for Pod1 , Gene for Pod2 ) sorted by fitness (the head is the best individual)  
type Population = [(Gene,Gene)]

-- | A simple GA player (no configuring parameter for now)
data GASimple = GASimple
instance PlayerIO GASimple where
  playerInitIO _ = undefined
  playerRunIO _ PlayerIn{selfPod=[p1,p2],oppoPod=[o1,o2]} = do
    t0 <- getCPUTime 
    pop0 <- randomPop 50
    popFinal <- evolve t0 [p1,p2,o1,o2] pop0
    return (decodeGene (p1,p2) $ head popFinal , undefined)
    where
      maxTime = 60000000 ::Integer -- maximum time before returning the final answer 
      decodeGene :: (PodState,PodState) -> (Gene,Gene) -> PodMovement
      decodeGene (ps1,ps2) (g1,g2) = [decodeStep ps1 $ head g1 , decodeStep ps2 $ head g2]
      evolve :: Integer -> [PodState]-> Population -> IO Population
      evolve t0 ps@[p1,p2,o1,o2] g0 = do
        t1 <- getCPUTime
        if (t1-t0)>maxTime then return g0 else do
          mg1 <- timeout 4 (nextGen ps g0)
          case mg1 of
            Just  g1 -> evolve t0 ps g1
            Nothing  -> return g0
      
      
-- | Decode a Gene as PodMovement
decodeStep :: PodState -> Step -> PodMovement
decodeStep PodState{podPosition=pos} (ang,thrust) = 
  PodMovement (pos + V.roundVec (5000 `V.scalarMul` V.unitVec ang )) thrust

randomPop  :: Int -> IO Population
randomPop popSize =
  let randomPair = (,) <$> randomGene <*> randomGene
  in  sequence $ replicate popSize randomPair

randomGene :: Int -> IO Gene
randomGene geneLength = sequence $ replicate geneLength randomGene
  where randomGene = do
          delAngle <- randomRIO (-18.0,18.0)::IO Double
          n        <- randomRIO (0,39) :: Double
          let thrust = case n of
                _ | 0<=n  && n<10 -> Normal 0
                  | 10<=n && n<20 -> Normal 50
                  | 20<=n && n<30 -> Normal 100
                  | 30<=n && n<35 -> Boost
                  | 35<=n && n<40 -> Shield
          return (delAngle,thrust)


nextGen :: [PodState] -> Population -> IO Population
nextGen podStates ps = do
  let popSize = length ps
  let [p1,p2] = take 2 ps
  matingPool <- select popSize ps 
  childrens  <- crossoverAndMutate matingPool
  return $ take popSize $ sortOn (fitness podStates) (p1:p2:childrens)
    where
      select :: Int -> Population -> IO Population
      select n ps = randomPerm $ take n $ cycle $ take (n/2) ps  
      crossoverAndMutate :: Population -> IO Population -- length population should be even 
      crossoverAndMutate (g1:g2:gs) = do
            (g1', g2') <- crossover g1 g2 
            (g1'',g2'')<- (mutate g1,mutate g2)
            gs'' <- crossoverAndMutate gs
            return (g1'':g2'':gs'')
-- | take step number and a list of 2 PodStates and update the "PodMovement" of the two pods
type Driver = Int -> [PodState]->[PodState]
dummyOpponent :: Driver  
dummyOpponent = const id

makeDriver :: (Gene,Gene) -> Driver
makeDriver (g1,g2) n [p1,p2] = [p1{podMovement = decodeStep p1 (g1!!n)},p2{podMovement = decodeStep p2 (g2!!n)}

simTeam :: Int -> (Driver,Driver) -> [PodState] -> [PodState]
simTeam 0 (player ,opponent) psNow = psNow
simTeam n (player ,opponent) psNow =
  let ps' = [player (n-1) (take 2 psNow) ++ opponent (n-1) (drop 2 psNow)]
  in  simTeam (n-1) $ gameSimTurns 1 ps'
                                   
-- | The higher the score , the closer the Destination
-- | Int is the length of the up comming checkpoints
-- | double is the distance to the next checkpoint
data PodScore = (Int,Double)
instance Num PodScore where
  (+) (i1,d1) (i2,d2) = (i1+i2,d1+d2)
  (-) (i1,d1) (i2,d2) = (i1-i2,d1-d2)
  abs _         = error "undefine  operation 'abs' for PodScore"
  (*) _ _       = error "undefined operation (*) for PodScore"
  signum _      = error "undefined operation 'signum' for PodScore"
  fromInteger _ = error "not defined"

-- | turn PodScore into Double for compare
-- | the higher the score , the closer to the destination
-- | highest possible score is 0
measurePodScore :: Int -> PodScore -> Double
measurePodScore ckptWeight (i,d)=  - fromIntegral (ckptWeight * i) - d
instance Ord PodScore where
  compare ps1@(!i1,!d1) ps2@(!i2,!d2) = compare $ measurePodScore 16000 ps1 $ measurePodScore 16000 ps2

-- | Evalutating the score of a single  PodState
getPodScore :: PodState -> PodScore
getPodScore PodState{podPosition=pos,podNextCheckPoints = ckpts} = (length ckpts) 


-- | The Score of a simulation result of a pair of Genes
-- | Contains the PodScore of player's best Pod and opponent's best Pod  
data  TeamScore = TeamScore{selfBest::PodScore , oppoBest::PodScore} 
-- | turn TeamScore into Double for compare
-- | the higher the score , the better the team
measureTeamScore :: Double -> TeamScore -> Double
measureTeamScore k TeamScore{selfBest=s,oppoBesto1} = k * measurePodScore s +  measurePodScore o  
instance Ord TeamScore where
  compare ts1 ts2
    = compare $ measureTeamScore 1 ts1 $ measureTeamScore 1 ts2

-- | Calculate the fitness of a (Gene,Gene) for (pod1,pod2)
getTeamScore :: [PodState] -> TeamScore  
getTeamScore  [p1,p2,o1,o2] =
  TeamScore $ max (getPodScore p1) (getPodScore p2) $ max (getPodScore o1) (getPodScore o2)
    - if (podThrust podMovement p1 == Boost) then 10000 else 0
    - if (podThrust podMovement p2 == Boost) then 10000 else 0
