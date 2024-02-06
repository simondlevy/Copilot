{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Demands where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

data Demands = Demands { 
    throttle :: Field "throttle" Float 
  , roll     :: Field "roll" Float 
  , pitch    :: Field "pitch" Float 
  , yaw      :: Field "yaw" Float 
}

data StreamedDemands = StreamedDemands {

     streamedThrottle :: Stream Float
   , streamedRoll     :: Stream Float
   , streamedPitch    :: Stream Float
   , streamedYaw      :: Stream Float
}

liftDemands :: Stream Demands -> StreamedDemands
liftDemands demands = StreamedDemands (demands # throttle) 
                                      (demands # roll) 
                                      (demands # pitch) 
                                      (demands # yaw) 

instance Struct Demands where

    typename _ = "demands" -- Name of the type in C

    toValues v = [ Value Float (throttle v)
                 , Value Float (roll v)
                 , Value Float (pitch v)
                 , Value Float (yaw v)
                 ]

instance Typed Demands where

  typeOf = Struct (Demands (Field 0) (Field 0) (Field 0) (Field 0))

getDemands :: Stream Demands -> 
                (Stream Float, Stream Float, Stream Float, Stream Float)
getDemands demands = (demands # throttle, demands # roll, demands # pitch, demands # yaw)


