module Main where

import Monstick.Platforms
import Monstick.Test.RFMInit (app)

main :: IO ()
main = buildMonstickApp platform app
  where platform = monstick {
      basePlatform = (basePlatform monstick) { platformSPIDevs = [ mikrobus_rfm1, mikrobus_rfm2 ] }
    }
