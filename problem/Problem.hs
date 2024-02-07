{-# LANGUAGE DataKinds #-}

module Problem where

import Language.Copilot
import Copilot.Compile.C99

import Demands
import State

fun3 :: Stream Float -> Stream Float -> Stream Float
fun3 desired measured = desired - measured

fun2 :: Stream Float -> Stream Float -> Stream Float
fun2 desired measured = desired - measured

fun1 :: State -> Demands

fun1 state  = Demands thrust' 0 0 0

  where climbRate = fun3 0 (z state)

        thrust' = fun2 climbRate (dz state)
