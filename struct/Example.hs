{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

import Types

------------------------------------------------------------------------------

data Demands_ = Demands_ { 
    throttle_ :: Field "throttle" Float 
  , roll_     :: Field "roll" Float 
  , pitch_    :: Field "pitch" Float 
  , yaw_      :: Field "yaw" Float 
}

instance Struct Demands_ where

    typename _ = "demands" -- Name of the type in C

    toValues v = [ Value Float (throttle_ v)
                 , Value Float (roll_ v)
                 , Value Float (pitch_ v)
                 , Value Float (yaw_ v)
                 ]

instance Typed Demands_ where

  typeOf = Struct (Demands_ (Field 0) (Field 0) (Field 0) (Field 0))

demands :: Stream Demands_
demands = extern "demands" Nothing

convertDemands_ :: Demands_ -> Demands
convertDemands_  (Demands_ (Field t) (Field r) (Field p) (Field y)) = 
    Demands t r p y

------------------------------------------------------------------------------

data State_ = State_ { 
    x_      :: Field "x" Float 
  , dx_     :: Field "dx" Float 
  , y_      :: Field "y " Float 
  , dy_     :: Field "dy" Float 
  , z_      :: Field "z" Float 
  , dz_     :: Field "dz" Float 
  , phi_    :: Field "phi" Float 
  , dphi_   :: Field "dphi" Float 
  , theta_  :: Field "theta" Float 
  , dtheta_ :: Field "dtheta" Float 
  , psi_    :: Field "psi" Float 
  , dpsi_   :: Field "dpsi" Float 
}

instance Struct State_ where

    typename _ = "state" -- Name of the type in C

    toValues v = [ Value Float (x_ v)
                 , Value Float (dx_ v)
                 , Value Float (y_ v)
                 , Value Float (dy_ v)
                 , Value Float (z_ v)
                 , Value Float (dz_ v)
                 , Value Float (phi_ v)
                 , Value Float (dphi_ v)
                 , Value Float (theta_ v)
                 , Value Float (dtheta_ v)
                 , Value Float (psi_ v)
                 , Value Float (dpsi_ v)
                 ]

instance Typed State_ where

  typeOf = Struct (State_
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

state :: Stream State_
state = extern "state" Nothing

convertState_ :: State_ -> State
convertState_  (State_ (Field x) 
                       (Field dx) 
                       (Field y) 
                       (Field dy)
                       (Field z) 
                       (Field dz)
                       (Field phi) 
                       (Field dphi)
                       (Field theta) 
                       (Field dtheta)
                       (Field psi) 
                       (Field dpsi)
               ) = 

    State x dx y dy z dz phi dphi theta dtheta psi dpsi

-------------------------------------------------------------------------------



spec = do

  let t = demands # throttle_
  let r = demands # roll_
  let p = demands # pitch_
  let y = demands # yaw_

  let m1 = t - r + p  - y
  let m2 = t - r - p  + y
  let m3 = t + r + p  + y
  let m4 = t + r - p  - y

  trigger "run" true [arg m1, arg m2, arg m3, arg m4] 

-- Compile the spec
main = reify spec >>= compile "copilot"
