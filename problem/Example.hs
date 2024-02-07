{-# LANGUAGE DataKinds #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

data MyStruct = MyStruct { 
    x'      :: Field "x" Float 
  , y'      :: Field "y " Float 
  , z'      :: Field "z" Float 
}

data MyData = MyData { 
    x      :: Stream Float 
  , y      :: Stream Float 
  , z      :: Stream Float 
}

instance Struct MyStruct where

    typename _ = "state" -- Name of the type in C

    toValues v = [ Value Float (x' v)
                 , Value Float (y' v)
                 , Value Float (z' v)
                 ]

instance Typed MyStruct where

  typeOf = Struct (MyStruct
                   (Field 0)
                   (Field 0)
                   (Field 0)
                  )

liftMyData :: Stream MyStruct -> MyData
liftMyData state = MyData (state # x') 
                        (state # y') 
                        (state # z') 

fun3 :: Stream Float -> Stream Float -> Stream Float
fun3 desired measured = desired - measured

fun2 :: Stream Float -> Stream Float -> Stream Float
fun2 desired measured = desired - measured

fun1 :: MyData -> Stream Float

fun1 state  = o2

  where o3 = fun3 0 (z state)

        o2 = fun2 o3 (z state)

stateStruct :: Stream MyStruct
stateStruct = extern "state" Nothing

spec = do

  let value = fun1 (liftMyData stateStruct)

  trigger "run" true [arg $ value]

-- Compile the spec
main = reify spec >>= compile "copilot"
