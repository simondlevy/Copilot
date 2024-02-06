{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Mixers where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

import Demands
import Motors

type Mixer = Stream Demands -> Motors

getDemands :: Stream Demands -> 
                (Stream Float, Stream Float, Stream Float, Stream Float)
getDemands dmds = (dmds # throttle, dmds # roll, dmds # pitch, dmds # yaw)

quadAPMixer :: Mixer
quadAPMixer dmds = QuadMotors m1 m2 m3 m4
        where (t, r, p, y) = (getDemands dmds)
              m1 = t - r + p  - y
              m2 = t - r - p  + y
              m3 = t + r + p  + y
              m4 = t + r - p  - y

quadCFMixer :: Mixer
quadCFMixer dmds = QuadMotors m1 m2 m3 m4
        where (t, r, p, y) = (getDemands dmds)
              m1 = t - r + p  + y
              m2 = t - r - p  - y
              m3 = t + r - p  + y
              m4 = t + r + p  - y
