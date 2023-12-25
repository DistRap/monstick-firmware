{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

module Monstick.Test.RFMLoopback where

import Data.Char (ord)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched

import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.EXTI
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.EXTI

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

  [sxISR0, sxISR1] <- extiTower platformEXTI [
      EXTIPin (mbISR mikrobus1) Rising NoPull
    , EXTIPin (mbISR mikrobus2) Rising NoPull
    ]

  let defaultConfig' = defaultConfig { sx_polling_period = (Nothing :: Maybe Milliseconds) }
  (sxTask0, sxReq0) <- task "sx0"
  (rdy0, BackpressureTransmit txReq0 txDone0, BackpressureTransmit rxReq0 rxDone0) <-
    sxTower sxReq0 sready (SPIDeviceHandle 0) "left" (defaultConfig { sx_isr = Just sxISR0 }) (mbRST mikrobus1)

  (sxTask1, sxReq1) <- task "sx1"
  (rdy1, BackpressureTransmit txReq1 txDone1, BackpressureTransmit rxReq1 rxDone1) <-
    sxTower sxReq1 sready (SPIDeviceHandle 1) "right" (defaultConfig { sx_isr = Just sxISR1 }) (mbRST mikrobus2)

  togRed   <- ledToggle [platformRedLED]
  togGreen <- ledToggle [platformGreenLED]
  fwd txDone0 togRed
  fwd rxDone1 togGreen

  schedule "sxRadios"
    [sxTask0, sxTask1] sready sreq

   -- TODO: make SX LOOPBACK!! out of this
   -- add cnt
   -- add handler for rx checking expected msg
   -- puts tries 7 / 10
   -- puts test_ok after several
  --per <- period (Seconds 30)
  per <- period (Seconds 1)
  monitor "loop" $ do
    {--
    handler rdy1 "rdy1" $ do
      rxE0 <- emitter rxReq0 1
      rxE1 <- emitter rxReq1 1
      callback $ const $ do
        r <- local $ istruct [
            radio_listen_continuous .= ival true
          , radio_listen_frequency .= ival 868_100_000
          ]
        --emit rxE0 (constRef r)
        emit rxE1 (constRef r)
    --}
    --
    mp <- stateInit ("mainPeriodCount") (ival (0 :: Uint32))
    rdyTX <- state "rdyTX"
    rdyRX <- state "rdyRX"

    handler rdy0 "rdyTX" $ callback $ const $ store rdyTX true
    handler rdy1 "rdyRX" $ callback $ const $ store rdyRX true

    handler per "p" $ do
      rxE0 <- emitter rxReq0 1
      rxE1 <- emitter rxReq1 1

      txE0 <- emitter txReq0 1
      txE1 <- emitter txReq1 1

      let
          --freq = 433_750_000
          freq = 868_100_000

      callback $ const $ do
        isTXReady <- deref rdyTX
        isRXReady <- deref rdyRX
        when (isTXReady .&& isRXReady) $ do
          curmp <- deref mp
          mp += 1
          linkConf <- local $ istruct [
              radio_link_frequency .= ival freq
            , radio_link_coding_rate .= ival cr1
            , radio_link_bandwidth .= ival bw500
            , radio_link_spreading .= ival sf7
            , radio_link_iq_invert .= ival false
            ]

          r <- local $ istruct [
              radio_listen_continuous .= ival true
            ]
          refCopy (r ~> radio_listen_conf) linkConf

          --emit rxE0 (constRef r)
          when (curmp ==? 0) $ do
            emit rxE1 (constRef r)
            store rdyRX false

          t <- local $ istruct [
              radio_tx_buf .= istruct [
                 stringDataL .= iarray (map ival [0x1, 0x3, 0x3, 0x7])
               , stringLengthL .= ival 4
               ]
            ]
          refCopy (t ~> radio_tx_conf) linkConf
          emit txE0 (constRef t)
          return ()
          --emit txE1 (constRef t)


  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  monitor "u" $ do
    handler systemInit "su" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "OHAI\r\n"

    handler sxISR0 "sxISR0" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "ISR0\r\n"

    handler sxISR1 "sxISR1" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "ISR1\r\n"

    handler rdy0 "rdy0" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "-rdy0\r\n"

    handler rdy1 "rdy1" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "-rdy1\r\n"

    handler txDone0 "txDone0" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "txDone0\r\n"

    handler rxDone1 "rxDone1" $ do
      o <- emitter ostream 32
      callback $ const $ do
        puts o "rxDone1\r\n"
