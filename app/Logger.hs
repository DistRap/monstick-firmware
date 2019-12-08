module Main where

import Monstick.Platforms
import Monstick.App.Logger (app)

main :: IO ()
main = buildMonstickApp monstick app
