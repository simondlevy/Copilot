{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

-- import Types

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

------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

data Motors = QuadMotors { 
                       qm1 :: Stream Float
                     , qm2 :: Stream Float  
                     , qm3 :: Stream Float  
                     , qm4 :: Stream Float   
               } |

              HexMotors {
                    hm1 :: Stream Float  
                  , hm2 :: Stream Float  
                  , hm3 :: Stream Float  
                  , hm4 :: Stream Float  
                  , hm5 :: Stream Float  
                  , hm6 :: Stream Float  
               } deriving (Show)

-------------------------------------------------------------------------------

mix :: Stream Demands -> Motors
mix dmds = QuadMotors m1 m2 m3 m4
  where t = dmds # throttle
        r = dmds # roll
        p = dmds # pitch
        y = dmds # yaw
        m1 = t - r + p  - y
        m2 = t - r - p  + y
        m3 = t + r + p  + y
        m4 = t + r - p  - y

demands :: Stream Demands
demands = extern "demands" Nothing

state :: Stream State
state = extern "state" Nothing

spec = do

  let t = demands # throttle
  let r = demands # roll
  let p = demands # pitch
  let y = demands # yaw

  let m1 = t - r + p  - y
  let m2 = t - r - p  + y
  let m3 = t + r + p  + y
  let m4 = t + r - p  - y

  let motors = mix demands

  trigger "run" true [
                       arg $ qm1 motors, 
                       arg $ qm2 motors, 
                       arg $ qm3 motors, 
                       arg $ qm4 motors
                     ] 

-- Compile the spec
main = reify spec >>= compile "copilot"
