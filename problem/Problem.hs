{-# LANGUAGE DataKinds #-}

module Problem where

import Language.Copilot
import Copilot.Compile.C99

import State

fun3 :: Stream Float -> Stream Float -> Stream Float
fun3 desired measured = desired - measured

fun2 :: Stream Float -> Stream Float -> Stream Float
fun2 desired measured = desired - measured

fun1 :: State -> Stream Float

fun1 state  = thrust'

  where climbRate = fun3 0 (z state)

        thrust' = fun2 climbRate (dz state)
