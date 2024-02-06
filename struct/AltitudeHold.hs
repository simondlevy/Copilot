{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module AltitudeHold where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State
import Clock

type Controller = ClockRate -> State -> Demands -> Demands

altitudePid :: Stream Float -> Stream Float -> Stream Float
altitudePid _ _ = 0

altitudeHold :: Controller

altitudeHold updateRate state (Demands thrust roll pitch yaw) = 
  Demands thrust roll pitch yaw

  where altitudeKp = 2.0
        altitudeKi = 0.5
        climbRateKp = 25.0
        climbRateKi = 15.0

        dt = rateToPeriod updateRate
        dz' = dz state

        climbRate = altitudePid thrust (z state)
