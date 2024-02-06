{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

------------------------------------------------------------------------------

data Demands = Demands { 
    throttle :: Field "throttle" Float 
  , roll     :: Field "roll" Float 
  , pitch    :: Field "pitch" Float 
  , yaw      :: Field "yaw" Float 
}

instance Struct Demands where

    typename _ = "demands" -- Name of the type in C

    toValues v = [ Value Float (throttle v)
                 , Value Float (roll v)
                 , Value Float (pitch v)
                 , Value Float (yaw v)
                 ]

instance Typed Demands where

  typeOf = Struct (Demands (Field 0) (Field 0) (Field 0) (Field 0))

demands :: Stream Demands
demands = extern "demands" Nothing

------------------------------------------------------------------------------

data State = State { 
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

instance Struct State where

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

instance Typed State where

  typeOf = Struct (State 
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

state :: Stream State
state = extern "state" Nothing

------------------------------------------------------------------------------

spec = do

  let t = demands # throttle
  let r = demands # roll
  let p = demands # pitch
  let y = demands # yaw

  let m1 = t - r + p  - y
  let m2 = t - r - p  + y
  let m3 = t + r + p  + y
  let m4 = t + r - p  - y

  trigger "run" true [arg m1, arg m2, arg m3, arg m4] 

-- Compile the spec
main = reify spec >>= compile "copilot"
