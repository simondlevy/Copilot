{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Problem where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State

------------------------------------------------------------------------------

altitudePid :: Stream Float -> Stream Float -> Stream Float

altitudePid desired measured = kp * error + ki * errorIntegral

  where kp = 2.0
        ki = 0.5
        integralLimit = 5000.0

        error = desired - measured

        errorIntegral = 0

------------------------------------------------------------------------------

climbRatePid :: Stream Float -> Stream Float -> Stream Float

climbRatePid desired measured = kp * error + ki * errorIntegral

  where kp = 2.0
        ki = 0.5
        integralLimit = 5000.0

        error = desired - measured

        errorIntegral = 0 

------------------------------------------------------------------------------

altitudeHold state (Demands thrust roll pitch yaw) = 
  Demands thrust' roll pitch yaw

  where climbRate = altitudePid thrust (z state)

        thrust' = climbRatePid climbRate (dz state)
