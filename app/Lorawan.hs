module Main where

import Monstick.Platforms
import Monstick.App.Lorawan (app)

main :: IO ()
main = buildMonstickApp monstick app
