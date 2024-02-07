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

    typename _ = "mystruct" -- Name of the type in C

    toValues v = [ Value Float (x' v) , Value Float (y' v) , Value Float (z' v) ]

instance Typed MyStruct where

  typeOf = Struct (MyStruct (Field 0) (Field 0) (Field 0))

liftMyData :: Stream MyStruct -> MyData
liftMyData mydata = MyData (mydata # x') (mydata # y') (mydata # z') 

fun3 :: Stream Float -> Stream Float
fun3 a = a

fun2 :: Stream Float -> Stream Float -> Stream Float
fun2 b a = b - a

fun1 :: MyData -> Stream Float

fun1 mydata  = o2

  where o3 = fun3 (z mydata)

        o2 = fun2 o3 (z mydata)

mydataStruct :: Stream MyStruct
mydataStruct = extern "mydata" Nothing

spec = do

  let value = fun1 (liftMyData mydataStruct)

  trigger "run" true [arg $ value]

-- Compile the spec
main = reify spec >>= compile "copilot"
