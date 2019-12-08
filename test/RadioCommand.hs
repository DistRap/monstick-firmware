module Main where

import Monstick.Platforms
import Monstick.Test.RadioCommand (app)

main :: IO ()
main = buildMonstickApp monstick app
