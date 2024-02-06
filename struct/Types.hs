module Types where

data Demands = Demands { 
    throttle :: Float 
  , roll     :: Float 
  , pitch    :: Float 
  , yaw      :: Float 
}

data Motors = QuadMotors { 
                       qm1 :: Float
                     , qm2 :: Float  
                     , qm3 :: Float  
                     , qm4 :: Float   
               } |

              HexMotors {
                    hm1 :: Float  
                  , hm2 :: Float  
                  , hm3 :: Float  
                  , hm4 :: Float  
                  , hm5 :: Float  
                  , hm6 :: Float  
               } deriving (Show)


type Mixer = Demands -> Motors

quadXAPMixer :: Mixer
quadXAPMixer demands = 
    let t = (throttle demands)
        r = (roll demands)
        p = (pitch demands)
        y = (yaw demands)
    in QuadMotors (t - r - p - y)
                  (t + r + p - y)
                  (t + r - p + y)
                  (t - r + p + y)
 


