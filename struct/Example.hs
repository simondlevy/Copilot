{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

import Demands
import State
import Motors

-------------------------------------------------------------------------------

type Mixer = Stream Demands -> Motors

quadAPMixer :: Mixer

quadAPMixer dmds = QuadMotors m1 m2 m3 m4
  where t = dmds # throttle
        r = dmds # roll
        p = dmds # pitch
        y = dmds # yaw
        m1 = t - r + p  - y
        m2 = t - r - p  + y
        m3 = t + r + p  + y
        m4 = t + r - p  - y

-------------------------------------------------------------------------------

demands :: Stream Demands
demands = extern "demands" Nothing

state :: Stream State
state = extern "state" Nothing

spec = do

  let motors = quadAPMixer demands

  trigger "run" true [
                       arg $ qm1 motors, 
                       arg $ qm2 motors, 
                       arg $ qm3 motors, 
                       arg $ qm4 motors
                     ] 

-- Compile the spec
main = reify spec >>= compile "copilot"
