{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module AltitudeHold where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State
import Clock

smax :: Stream Float -> Stream Float -> Stream Float
smax x y = if x > y then x else y

type Controller = ClockRate -> State -> Demands -> Demands

altitudePid :: Stream Float -> Stream Float -> Stream Float -> Stream Float

altitudePid dt desired measured = kp * error + ki * errorIntegral

  where kp = 2.0
        ki = 0.5
        integralLimit = 5000.0

        error = desired - measured

        errorIntegral = smax (errorIntegral' + error) integralLimit

        errorIntegral' = [0] ++ errorIntegral

altitudeHold :: Controller

altitudeHold updateRate state (Demands thrust roll pitch yaw) = 
  Demands thrust' roll pitch yaw

  where climbRateKp = 25.0
        climbRateKi = 15.0

        dt = rateToPeriod updateRate
        dz' = dz state

        climbRate = altitudePid dt thrust (z state)

        thrust' = thrust
