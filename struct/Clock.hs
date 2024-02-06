{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Clock where

import Language.Copilot
import Copilot.Compile.C99

data ClockRate = 
              RATE_25_HZ
            | RATE_30_HZ
            | RATE_33_HZ
            | RATE_50_HZ
            | RATE_100_HZ
            | RATE_250_HZ
            | RATE_500_HZ
            | RATE_1000_HZ

rateToPeriod :: ClockRate -> Float
rateToPeriod RATE_25_HZ   = 25
rateToPeriod RATE_30_HZ   = 30
rateToPeriod RATE_33_HZ   = 33
rateToPeriod RATE_50_HZ   = 50
rateToPeriod RATE_100_HZ  = 100
rateToPeriod RATE_250_HZ  = 250
rateToPeriod RATE_500_HZ  = 500
rateToPeriod RATE_1000_HZ = 1000

