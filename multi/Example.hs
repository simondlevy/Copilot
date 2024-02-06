{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99
import Prelude hiding((>))

data Demands = Demands { throttle :: Float
                       , roll :: Float  
                       , pitch :: Float  
                       , yaw :: Float  
                     } 

demands :: Stream Demands
demands = extern "demands" Nothing

t :: Stream Float
t  = extern "t" Nothing

r :: Stream Float
r  = extern "r" Nothing

p :: Stream Float
p  = extern "p" Nothing

y :: Stream Float
y  = extern "y" Nothing

rescale :: Stream Float -> Stream Float -> Stream Float


-- XXX Un-comment one of these; comment the other
rescale mx v = if mx > 1 then v - mx + 1 else v
-- rescale mx v = v 

spec = do

  let m1 = t - r + p  - y
  let m2 = t - r - p  + y
  let m3 = t + r + p  + y
  let m4 = t + r - p  - y

  let mmax = foldr fmax m1 [m2, m3, m4] where fmax a b = if a > b then a else b

  let m1' = rescale mmax m1
  let m2' = rescale mmax m2
  let m3' = rescale mmax m3
  let m4' = rescale mmax m4

  trigger "run" true [arg m1', arg m2', arg m3', arg m4'] 

-- Compile the spec
main = reify spec >>= compile "copilot"
