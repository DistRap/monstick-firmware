{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

module Monstick.Test.RFMInit where

import Data.Char (ord)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched

import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Driver.SPI

import Ivory.Base
import Ivory.Tower.Base

import Ivory.Tower.Drivers.Net.SX127x

import Monstick.Platforms
import Monstick.Types

app :: (e -> ClockConfig) -> (e -> MonstickPlatform) -> Tower e ()
app tocc toPlatform = do
  monstickTowerDeps

  MonstickPlatform{..} <- fmap toPlatform getEnv
  let Platform{..} = basePlatform

  (sreq, sready) <- spiTower tocc platformSPIDevs platformSPIPins

  (sxTask0, sxReq0) <- task "sx0"
  (rdy0, BackpressureTransmit txReq txDone, _) <-
    sxTower sxReq0 sready (SPIDeviceHandle 0) "left" defaultConfig (mbRST mikrobus1)

  return ()
  (sxTask1, sxReq1) <- task "sx1"
  (rdy1, _, BackpressureTransmit rxRes rxDone) <-
    sxTower sxReq1 sready (SPIDeviceHandle 1) "right" defaultConfig (mbRST mikrobus2)

  togRed   <- ledToggle [platformRedLED]
  togGreen <- ledToggle [platformGreenLED]
  fwd rdy0 togRed
  fwd rdy1 togGreen

  schedule "sxRadios"
    [sxTask0, sxTask1] sready sreq

  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  monitor "u" $ do
    handler systemInit "su" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "Init start\r\n"

    handler rdy0 "i0done" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "Radio 0 init done\r\n"

    handler rdy1 "i1done" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "Radio 1 init done\r\n"
