module Main where

import Language.Copilot
import Copilot.Compile.C99

import State

import Problem

stateStruct :: Stream StateStruct
stateStruct = extern "state" Nothing

spec = do

  let value = fun1 (liftState stateStruct)

  trigger "run" true [arg $ value]

-- Compile the spec
main = reify spec >>= compile "copilot"
