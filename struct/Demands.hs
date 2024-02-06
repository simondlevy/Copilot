{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Demands where

import Language.Copilot
import Copilot.Compile.C99

data Demands' = Demands' { 
    throttle' :: Field "throttle" Float 
  , roll'     :: Field "roll" Float 
  , pitch'    :: Field "pitch" Float 
  , yaw'      :: Field "yaw" Float 
}

data Demands = Demands {

     throttle :: Stream Float
   , roll     :: Stream Float
   , pitch    :: Stream Float
   , yaw      :: Stream Float
}

liftDemands :: Stream Demands' -> Demands
liftDemands demands = Demands (demands # throttle') 
                              (demands # roll') 
                              (demands # pitch') 
                              (demands # yaw') 

instance Struct Demands' where

    typename _ = "demands" -- Name of the type in C

    toValues v = [ Value Float (throttle' v)
                 , Value Float (roll' v)
                 , Value Float (pitch' v)
                 , Value Float (yaw' v)
                 ]

instance Typed Demands' where

  typeOf = Struct (Demands' (Field 0) (Field 0) (Field 0) (Field 0))

getDemands :: Demands -> 
                (Stream Float, Stream Float, Stream Float, Stream Float)

getDemands demands = (throttle demands, roll demands, pitch demands, yaw demands)
