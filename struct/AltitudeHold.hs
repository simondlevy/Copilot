{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module AltitudeHold where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

import Demands
import State

type Controller = Stream State -> Demands -> Demands

altitudeHold :: Controller
altitudeHold _ (Demands t r p y) = Demands t r p y
