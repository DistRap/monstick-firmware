module Main where

import Monstick.Platforms
import Monstick.Test.ADS (app)

main :: IO ()
main = buildMonstickApp monstick app
