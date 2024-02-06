{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

data Demands = Demands { 
    t :: Field "t" Float 
  , r :: Field "r" Float 
  , p :: Field "p" Float 
  , y :: Field "y" Float 
}

instance Struct Demands where

    typename _ = "demands" -- Name of the type in C

    toValues v = [ Value Float (t v)
                 , Value Float (r v)
                 , Value Float (p v)
                 , Value Float (y v)
                 ]

instance Typed Demands where

  typeOf = Struct (Demands (Field 0) (Field 0) (Field 0) (Field 0))

demands :: Stream Demands
demands = extern "demands" Nothing

spec = do

  trigger "split" true [arg $ demands # r, arg $ demands # p]

-- Compile the spec
main = reify spec >>= compile "copilot"

