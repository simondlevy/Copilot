{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module AltitudeHold where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State
import Clock

type Controller = ClockRate -> State -> Demands -> Demands

altitudePid :: Stream Float -> Stream Float -> Stream Float -> Stream Float

altitudePid dt desired measured = 0

  where kp = 2.0
        ki = 0.5

        error = desired - measured

altitudeHold :: Controller

altitudeHold updateRate state (Demands thrust roll pitch yaw) = 
  Demands thrust roll pitch yaw

  where climbRateKp = 25.0
        climbRateKi = 15.0

        dt = rateToPeriod updateRate
        dz' = dz state

        climbRate = altitudePid dt thrust (z state)
