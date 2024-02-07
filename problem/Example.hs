{-# LANGUAGE DataKinds #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

-- Structure shared with C ---------------------------------------

data MyStruct = MyStruct { 
    x'      :: Field "x" Float 
  , y'      :: Field "y " Float 
  , z'      :: Field "z" Float 
}

instance Struct MyStruct where

    typename _ = "mystruct" -- Name of the type in C

    toValues v = [ 
                    Value Float (x' v)
                  , Value Float (y' v) 
                  , Value Float (z' v) 
                 ]

instance Typed MyStruct where

  typeOf = Struct (MyStruct 
                     (Field 0) 
                     (Field 0) 
                     (Field 0)
                  )

-- Structure used internally -------------------------------------

data MyData = MyData { 
    x      :: Stream Float 
  , y      :: Stream Float 
  , z      :: Stream Float 
}

------------------------------------------------------------------

fun :: MyData -> Stream Float
fun mydata  = z mydata

mystruct :: Stream MyStruct
mystruct = extern "mydata" Nothing

spec = do

  let mydata = MyData (mystruct # x') (mystruct # y') (mystruct # z') 

  let value = fun mydata

  trigger "run" true [arg $ value]

-- Compile the spec
main = reify spec >>= compile "copilot"
