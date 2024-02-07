{-# LANGUAGE DataKinds #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

data StateStruct = StateStruct { 
    x'      :: Field "x" Float 
  , y'      :: Field "y " Float 
  , dy'     :: Field "dy" Float 
  , z'      :: Field "z" Float 
}

data State = State { 
    x      :: Stream Float 
  , y      :: Stream Float 
  , dy     :: Stream Float 
  , z      :: Stream Float 
}

instance Struct StateStruct where

    typename _ = "state" -- Name of the type in C

    toValues v = [ Value Float (x' v)
                 , Value Float (y' v)
                 , Value Float (dy' v)
                 , Value Float (z' v)
                 ]

instance Typed StateStruct where

  typeOf = Struct (StateStruct
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                  )

liftState :: Stream StateStruct -> State
liftState state = State (state # x') 
                        (state # y') 
                        (state # dy') 
                        (state # z') 

fun3 :: Stream Float -> Stream Float -> Stream Float
fun3 desired measured = desired - measured

fun2 :: Stream Float -> Stream Float -> Stream Float
fun2 desired measured = desired - measured

fun1 :: State -> Stream Float

fun1 state  = o2

  where o3 = fun3 0 (z state)

        o2 = fun2 o3 (z state)

stateStruct :: Stream StateStruct
stateStruct = extern "state" Nothing

spec = do

  let value = fun1 (liftState stateStruct)

  trigger "run" true [arg $ value]

-- Compile the spec
main = reify spec >>= compile "copilot"
