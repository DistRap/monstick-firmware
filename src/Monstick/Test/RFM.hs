{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

-- due to wanState
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monstick.Test.RFM where

import Data.Char (ord)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched
-- RNG
import Ivory.HW

import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.EXTI
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Driver.EXTI
--import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.RNG

import Ivory.Base
import Ivory.Tower.Base

import Ivory.Tower.Drivers.Temperature.HTS221
import Ivory.Tower.Drivers.Temperature.Types
import Ivory.Tower.Drivers.Display.MAX7219
import Ivory.Tower.Drivers.Net.SX127x

import Ivory.Tower.LoraWAN
import Ivory.Tower.LoraWAN.Keys
import Ivory.Tower.LoraWAN.Pack
import Ivory.Tower.LoraWAN.Types
import Ivory.Tower.LoraWAN.Import
--import Ivory.Tower.LoraWAN.Regs
--import Ivory.Tower.LoraWAN.Major
--import Ivory.Tower.LoraWAN.MType

import Monstick.Types
import Monstick.Platforms

-- XXX
import Ivory.BSP.STM32L431
import Ivory.BSP.STM32L431.RCC

newtype WanState = WanState Uint8
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal)

wanIdle         :: WanState
wanIdle         = WanState 0
wanTransmitting :: WanState
wanTransmitting = WanState 1
wanReceiving    :: WanState
wanReceiving    = WanState 2
wanJoinRequest  :: WanState
wanJoinRequest  = WanState 3
wanJoinAccept   :: WanState
wanJoinAccept   = WanState 4
wanError        :: WanState
wanError        = WanState 4
wanWaitingConfirmation    :: WanState
wanWaitingConfirmation    = WanState 5

-- lorawan msg
--   link:
--     datarate
--     codingrate
--     power
--
--   data:
--     fport
--     msg string

app :: (e -> ClockConfig)
    -> (e -> MonstickPlatform)
    -> Tower e ()
app tocc toPlatform = do
  loraWANDeps
  monstickTowerDeps

  MonstickPlatform{..} <- fmap toPlatform getEnv
  let Platform{..} = basePlatform

  (sreq, sready) <- spiTower tocc platformSPIDevs platformSPIPins
  [sxISR] <- extiTower platformEXTI [
      EXTIPin (mbISR mikrobus1) Rising NoPull
    ]
  rngOut <- rngTower rng (Milliseconds 500)

  togRed   <- ledToggle [platformRedLED]
  togGreen <- ledToggle [platformGreenLED]

  (rdy , BackpressureTransmit txReq txDone , BackpressureTransmit rxReq rxDone) 
    <- sxTower
         sreq
         sready
         (SPIDeviceHandle 0)
         "left"
         (defaultConfig {
             sx_sync_word = ttnSyncWord
           , sx_isr = Just sxISR
         })
         (mbRST mikrobus1)

  fwd txDone togRed
  fwd rxDone togGreen

  p <- period (Seconds 1)
  rxPeriod <- period (Milliseconds 10)

  (ostream, istream) <- bufferedUartTower tocc platformUART platformUARTPins 115200 (Proxy :: Proxy UARTBuffer)
  monitor "u" $ do
    handler systemInit "su" $ do
      o <- emitter ostream 32
      callback $ const $ do
        -- needed to select correct RNG clock (sysclk)
        modifyReg rcc_reg_ccipr $ setField rcc_ccipr_clk48sel (fromRep 0b11)

        puts o "OHAI\r\n"

    --(buf :: Ref 'Global SXBuffer) <- state "buf"
    buf <- state "buf"
    radioReady <- state "radioReady"

    let
          freq = 868_100_000
          WANRegion{..} = eu868TTN

    linkConf <- stateInit "linkConf" (istruct [
              radio_link_frequency .= ival freq
            , radio_link_coding_rate .= ival cr1
            , radio_link_bandwidth .= ival bw125
            , radio_link_spreading .= ival sf7
            , radio_link_iq_invert .= ival false
            ])

    tryRx <- state "tryRx"
    inRX1 <- state "inRX1"
    inRX2 <- state "inRX2"
    joinRx <- state "joinRx"

    lastTx <- state "lastTx"
    handler txDone "txDone" $ do
      rxE <- emitter rxReq 1
      callback $ const $ do
        t <- getTime
        store lastTx t
        store tryRx true

    dbgCt <- state "dbgCt"
    handler rxPeriod "rxPeriod" $ do
      rxE <- emitter rxReq 1
      -- debug
      o <- emitter ostream 130
      callbackV $ \ct -> do
        wantRx <- deref tryRx
        when wantRx $ do
          store dbgCt ct

          lt <- deref lastTx
          rx1 <- deref inRX1
          rx2 <- deref inRX2
          jrx <- deref joinRx

          rx1delay <- assign
            $ jrx ? (toITime wanRegionJoinAcceptDelay1, toITime wanRegionReceiveDelay1)

          rx2delay <- assign
            $ jrx ? (toITime wanRegionJoinAcceptDelay2, toITime wanRegionReceiveDelay2)

          when (ct >=? lt + rx1delay .&& iNot rx1 .&& iNot rx2) $ do
            puts o "opening receive win #1"
            store inRX1 true
            r <- local $ istruct [ radio_listen_continuous .= ival false ]
            store (linkConf ~> radio_link_iq_invert) true
            -- same as last transmission
            refCopy (r ~> radio_listen_conf) linkConf
            emit rxE (constRef r)

          when (ct >=? lt + rx2delay .&& iNot rx2) $ do
            puts o "opening receive win #2"
            store inRX1 false
            store inRX2 true
            r <- local $ istruct [ radio_listen_continuous .= ival false ]
            store (linkConf ~> radio_link_iq_invert) true

            -- region specific rx2 window
            store (linkConf ~> radio_link_frequency) (fromIntegral wanRegionRX2Frequency)
            let DataRate _ rx2sf rx2bw = wanRegionRX2DataRate
            store (linkConf ~> radio_link_spreading) rx2sf
            store (linkConf ~> radio_link_bandwidth) rx2bw
            refCopy (r ~> radio_listen_conf) linkConf
            emit rxE (constRef r)

    handler rdy "radioReady" $ do
      callback $ const $ do
        store radioReady true

    lastRx <- state "lastRx"
    doneRx <- state "doneRx"
    handler rxDone "rxDone" $ do
      callback $ \rxRes -> do
        refCopy lastRx rxRes
        store doneRx true
        --
        store tryRx false
        store inRX1 false
        store inRX2 false

    nonce <- state "nonce"
    rngDone <- state "rngDone"
    handler rngOut "rng" $ do
      callbackV $ \x -> do
        done <- deref rngDone
        unless done $ do
          store nonce $ bitCast x
          store rngDone true
          rngRCCDisable rng

    wanState <- stateInit "wanState" (ival wanJoinRequest)

    nwkKey <- stateInit "nwkkey"
      $ iarray
      $ map ival [ 0xE2, 0xCD, 0x75, 0xC8, 0xBD, 0xCB, 0xE7, 0xA1, 0x5C, 0x34, 0x64, 0x48, 0x99, 0xD3, 0xDA, 0x09 ]

    joinacc <- state "joinacc"
    joined <- state "joined"
    micInvalid <- state "micInvalid"

    downmsg <- state "downmsg"

    -- network session key, derived when OTAA, can be provided for ABP
    nwkSKey <- state "nwks"
    -- application session key, derived when OTAA, can be provided for ABP
    appSKey <- state "apps"

    t <- state "txreq"
    --fcnt <- local $ ival (0 :: FCNT)
    fcnt <- state "fcnt"

    handler p "p" $ do
      o <- emitter ostream 130
      txE <- emitter txReq 1
      rxE <- emitter rxReq 1

      callback $ const $ do
        let transmit buffer = do
              refCopy (t ~> radio_tx_buf) buffer

              --- XXX: weird but we need to switch it back from RX2 window
              store (linkConf ~> radio_link_frequency) freq
              store (linkConf ~> radio_link_spreading) sf7
              store (linkConf ~> radio_link_bandwidth) bw125
              store (linkConf ~> radio_link_iq_invert) false

              refCopy (t ~> radio_tx_conf) linkConf
              emit txE (constRef t)

        puts o "."
        ready <- deref radioReady
        rngReady <- deref rngDone
        hasRx <- deref doneRx

        when (ready .&& rngReady) $ do
          st <- deref wanState
          when (st ==? wanJoinRequest) $ do
            puts o "Sending join request\r\n"

            nonce' <- deref nonce
            let joinEUI = 0x70B3D57ED002695F
                devEUI = 0x00F9F42F46DA72DD

            joinRequest
              buf
              (constRef nwkKey)
              joinEUI
              devEUI
              nonce'

            --t <- local $ istruct []
            transmit buf
            -- dbg
            putHexIvoryString o (constRef buf)
            puts o "\r\n"

            store joinRx true
            store wanState wanJoinAccept
            --store wanState wanIdle

          when (st ==? wanJoinAccept .&& hasRx) $ do
            store joinRx false
            puts o "Join accept:\r\n"
            putHexIvoryString o (constRef $ lastRx ~> radio_rx_buf)
            puts o "\r\n"

            len <- lastRx ~> radio_rx_buf ~>* stringLengthL
            (valid, join) <- fromJoinAccept
              (constRef (lastRx ~> radio_rx_buf ~> stringDataL))
              (constRef nwkKey)
              (signCast len)

            -- not really dbg
            refCopy joinacc join

            isValid <- deref valid
            ifte_ isValid
              (do
                puts o "- valid\r\n"
                store joined true

                nwks <- deriveKeyFNwkSInt (constRef nwkKey) (constRef nonce) (constRef joinacc)
                apps <- deriveKeyAppS     (constRef nwkKey) (constRef nonce) (constRef joinacc)
                refCopy nwkSKey nwks
                refCopy appSKey apps

                -- emit wanReady
                -- store wanState wanIdle
                -- TMP
                store wanState wanTransmitting
              )
              (do
                puts o "- invalid\r\n"
                store joined false
                store micInvalid true
                store wanState wanError
              )
            return ()

          when (st ==? wanTransmitting) $ do
            -- PAYLOAD
            plain <- local $ izero
            store (plain ! 0) 0x13
            store (plain ! 1) 0x37
            -- payload length
            upLen <- local $ ival $ 2

            let confirmed = true
            uplink
              buf
              (constRef appSKey)
              (constRef nwkSKey)
              confirmed
              0x13 -- FPort
              (constRef $ joinacc ~> acc_dev_addr)
              (constRef fcnt)
              (constRef plain)
              (constRef upLen)

            transmit buf
            puts o "Up\r\n"
            putHexIvoryString o (constRef buf)
            puts o "\r\n"

            --store wanState wanIdle
            store doneRx false
            store wanState wanWaitingConfirmation

          when (st ==? wanWaitingConfirmation .&& hasRx) $ do
            puts o "Down\r\n"
            putHexIvoryString o (constRef $ lastRx ~> radio_rx_buf)
            puts o "\r\n"

            len <- lastRx ~> radio_rx_buf ~>* stringLengthL
            (valid, mac) <- downlink
              (constRef (lastRx ~> radio_rx_buf ~> stringDataL))
              (signCast len)
              (constRef appSKey)
              (constRef nwkSKey)
              (constRef $ joinacc ~> acc_dev_addr)

            isValid <- deref valid
            refCopy downmsg mac
            ifte_ isValid
              (do
                puts o "- down valid\r\n"
              )
              (do
                puts o "- down invalid\r\n"
              )
            store wanState wanIdle
