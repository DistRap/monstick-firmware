{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monstick.Test.UARTBridge where

import Ivory.Language
import Ivory.Tower
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.GPIO

import Ivory.Tower.Base

import Monstick.Platforms
import Monstick.Types
import Monstick.VT100

app :: (e -> ClockConfig)
    -> (e -> Platform)
    -> Tower e ()
app tocc toPlatform = do
  monstickTowerDeps

  Platform{..} <- fmap toPlatform getEnv

  -- debug
  (ostream, istream) <- bufferedUartTower tocc
    platformUART platformUARTPins
    115200 (Proxy :: Proxy UARTBuffer)

  -- RN2483
  (ostreamRadio, istreamRadio) <- bufferedUartTower tocc
    (radioUART $ platformRadio)
    (radioUARTPins $ platformRadio)
    57600 (Proxy :: Proxy UARTBuffer)

  istreamLF <- appendCharAfter '\r' '\n' istream
  uartBridge ostream istreamLF ostreamRadio istreamRadio
  fwd istream ostream

  p <- period (Milliseconds 1000)
  delayedInit <- once p

  let rstPin = radioResetPin $ platformRadio
  monitor "radioEnabler" $ do
    monitorModuleDef $ hw_moduledef

    handler delayedInit "delayedInit" $ do
      callback $ const $ do
        pinSet rstPin

    handler systemInit "initResetPin" $ do
      o <- emitter ostream 64
      callback $ const $ do
        pinEnable rstPin
        pinSetMode rstPin gpio_mode_output
        pinClear rstPin

        puts o clearScreenCode
        puts o reset
