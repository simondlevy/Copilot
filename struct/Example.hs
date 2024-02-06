{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

-- import Prelude hiding ((>), (<), div, (++))

import AltitudeHold
import Clock
import Demands
import Mixers
import Motors
import State

demands' :: Stream Demands'
demands' = extern "demands" Nothing

state' :: Stream State'
state' = extern "state" Nothing

spec = do

  let motors = quadAPMixer $ altitudeHold  RATE_100_HZ 
                                          (liftState state') 
                                          (liftDemands demands')

  trigger "run" true [
                       arg $ qm1 motors, 
                       arg $ qm2 motors, 
                       arg $ qm3 motors, 
                       arg $ qm4 motors
                     ] 

-- Compile the spec
main = reify spec >>= compile "copilot"
