module Main where

import Monstick.Platforms
import Monstick.Test.Blink (app)

main :: IO ()
main = buildMonstickApp monstick app
