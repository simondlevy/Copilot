{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

data Vec = Vec { x :: Field "x" Float , y :: Field "y" Float }

instance Struct Vec where

    typename _ = "vec" -- Name of the type in C

    toValues v = [ Value Float (x v)
                 , Value Float (y v)
                 ]

instance Typed Vec where

  typeOf = Struct (Vec (Field 0) (Field 0))

vecs :: Stream Vec
vecs = [ Vec (Field 1) (Field 2) , Vec (Field 12) (Field 8) ] ++ vecs

spec = do
  trigger "split" true [arg $ vecs # x, arg $ vecs # y]

-- Compile the spec
main = reify spec >>= compile "copilot"

