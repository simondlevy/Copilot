{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module AltitudeHold where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State

type Controller = State -> Demands -> Demands

{--
altitudeHold :: Controller
altitudeHold state (Demands t r p y) = Demands t r p y
  where x' = state # x
--}

