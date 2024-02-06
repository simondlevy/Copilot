{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module State where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

data State = State { 
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

instance Struct State where

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


