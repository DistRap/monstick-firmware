-- | ADS1x1x I2C test
--
-- Periodically dumps ADC sampled values
-- to UART

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Monstick.Test.ADS where

import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Base
import Ivory.Tower.Drivers.ADC.ADS1x1x
import Monstick.Platforms
import Monstick.Types

app :: (a -> ClockConfig)
    -> (a -> MonstickPlatform)
    -> Tower a ()
app tocc toPlatform = do
  monstickTowerDeps

  MonstickPlatform{..} <- fmap toPlatform getEnv
  let Platform{..} = basePlatform

  (i2cTransmit, ready) <- i2cTower tocc platformI2C platformI2CPins
  togIn <- ledToggle [platformRedLED]

  (BackpressureTransmit adsRequest adsResult) <- adsTower i2cTransmit ready adsDefaultAddr

  per <- period (Milliseconds 1000)
  fwd per togIn

  ms100 <- period (Milliseconds 100)
  fwd ms100 adsRequest

  (ostream, _istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  monitor "dumpADS" $ do
    handler adsResult "adsResult" $ do
      o <- emitter ostream 64
      callback $ \adsSample -> do
        -- s   ser adc
        -- NO2 15K
        -- NH3 1M
        -- CO  1M
        -- series resistor 820
        -- r1 130
        -- r2 820
        -- r3 27
        arrayMap $ \i -> do
          c <- deref (adsSample ! i)
          (str :: Ref ('Stack s) UARTBuffer) <- uint32ToString $ safeCast c
          putIvoryString o (constRef str)
          puts o " "
        puts o "\r"
