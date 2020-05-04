{-# OPTIONS_GHC -O2  #-}
module Main

where

import qualified  Interact
import System.TimeIt

main :: IO ()
main = do
  -- (time,nTurn)<-timeItT Interact.main
  --putStrLn $ show $ (time,nTurn,fromIntegral nTurn / time)     
  Interact.testAnimate 5
     
    








