{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State

import Problem

demandsStruct :: Stream DemandsStruct
demandsStruct = extern "demands" Nothing

stateStruct :: Stream StateStruct
stateStruct = extern "state" Nothing

spec = do

  let demands = altitudeHold (liftState stateStruct) (liftDemands demandsStruct)

  trigger "run" true [arg $ throttle demands]

-- Compile the spec
main = reify spec >>= compile "copilot"
