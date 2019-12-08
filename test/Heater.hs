module Main where

import Monstick.Platforms
import Monstick.Test.Heater (app)

main :: IO ()
main = buildMonstickApp monstick app
