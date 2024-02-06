{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module State where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

data State' = State' { 
    x      :: Field "x" Float 
  , dx     :: Field "dx" Float 
  , y      :: Field "y " Float 
  , dy     :: Field "dy" Float 
  , z      :: Field "z" Float 
  , dz     :: Field "dz" Float 
  , phi    :: Field "phi" Float 
  , dphi   :: Field "dphi" Float 
  , theta  :: Field "theta" Float 
  , dtheta :: Field "dtheta" Float 
  , psi    :: Field "psi" Float 
  , dpsi   :: Field "dpsi" Float 
}

instance Struct State' where

    typename _ = "state" -- Name of the type in C

    toValues v = [ Value Float (x v)
                 , Value Float (dx v)
                 , Value Float (y v)
                 , Value Float (dy v)
                 , Value Float (z v)
                 , Value Float (dz v)
                 , Value Float (phi v)
                 , Value Float (dphi v)
                 , Value Float (theta v)
                 , Value Float (dtheta v)
                 , Value Float (psi v)
                 , Value Float (dpsi v)
                 ]

instance Typed State' where

  typeOf = Struct (State'
                   (Field 0) 
                   (Field 0) 
                   (Field 0) 
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                   (Field 0)
                  )
