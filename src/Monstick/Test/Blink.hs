{-# LANGUAGE RecordWildCards #-}

module Monstick.Test.Blink where

import Ivory.Tower
import Ivory.Tower.Base

import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Monstick.Platforms

app :: (e -> ClockConfig)
    -> (e -> MonstickPlatform)
    -> Tower e ()
app _tocc toPlatform = do
  Platform{..} <- fmap (basePlatform . toPlatform) getEnv
  blink (Milliseconds 1000) [platformRedLED]
  blink (Milliseconds 666) [platformGreenLED]

