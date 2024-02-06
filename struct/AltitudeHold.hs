{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module AltitudeHold where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State
import Clock

type Controller = ClockRate -> State -> Demands -> Demands

altitudeHold :: Controller
altitudeHold updateRate state (Demands t r p y) = Demands t r p y
  where x' = x state
        dt = rateToPeriod updateRate
        altitudeKp = 2.0
        altitudeKi = 0.5
        climbRateKp = 25.0
        climbRateKi = 15.0
