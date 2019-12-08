{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monstick.App.Lorawan where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Base

import Ivory.Tower.Drivers.Net.RN2483
import Ivory.Tower.Drivers.Temperature.SI7006
import Ivory.Tower.Drivers.Temperature.Types

import Ivory.Tower.Cayenne

import Monstick.Platforms
import Monstick.Types
import Monstick.VT100

app :: (e -> ClockConfig)
    -> (e -> Platform)
    -> Tower e ()
app tocc toPlatform = do
  cayenneTowerDeps
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

  -- pretty istream from RN2483
  istreamRadio' <- replaceChar '\r' '\n' istreamRadio
                   >>= dropEvery 2 (`isChar` '\n')

  (cIn, cOut) <- channel
  monitor "lineClear" $ do
    shouldClear <- stateInit "done" (ival true)
    handler istreamRadio' "i" $ do
      o <- emitter cIn 16
      callbackV $ \c -> do
        clr <- deref shouldClear
        cond_ [
            isChar c '\n' ==> store shouldClear true
          , clr ==> do
              puts o clearLineCode
              puts o "\r"
              store shouldClear false
          ]
        emitV o c

    handler systemInit "clearScreen" $ do
      o <- emitter cIn 16
      callbackV $ const $ do
        puts o clearScreenCode
        puts o reset

  -- UART echo
  fwd istream ostream

  -- screen only sends \r but radio needs \r\n
  istreamLF <- appendCharAfter '\r' '\n' istream
  uartBridge ostream istreamLF ostreamRadio cOut

  -- prepend RN2483 commands with >
  dbgstream <- dbgPrepend '\n'
    (clearLineCode ++ reset ++ "\r> ") ostream

  -- create merged input channel that feeds both
  -- ostream' and dbgstream
  merged <- ostreamRadio `mergeInputs` dbgstream

  (acc, _cmdMode, BackpressureTransmit cmd txdone) <- rn2483Tower
    (radioConfig $ platformRadio)
    merged istreamRadio systemInit
    (radioResetPin $ platformRadio)
    (Proxy :: Proxy UARTBuffer)

  (i2cTransmit, i2cReady) <- i2cTower tocc platformI2C platformI2CPins
  (BackpressureTransmit trigIn thOut) <- si7006Tower i2cTransmit i2cReady si7006DefaultAddr
  per <- period (Milliseconds 1000)
  fwd per trigIn

  msgPer <- period (minutes 3)

  monitor "rnleds" $ do
    monitorModuleDef $ hw_moduledef

    lastSample <- state "ls"
    dbg <- state "dbg"

    handler thOut "perSample" $ do
        o <- emitter ostream 64
        callback $ \sample -> do
          refCopy lastSample sample

          t <- sample ~>* sample_th_temperature
          h <- sample ~>* sample_th_humidity

          (strT :: Ref ('Stack s) UARTBuffer) <- floatingToString t 4
          (strH :: Ref ('Stack s) UARTBuffer) <- floatingToString h 4

          puts o clearLineCode
          puts o "\r"
          puts o bold
          puts o "T "
          puts o $ reset

          puts o $ color Red
          putIvoryString o (constRef strT)
          puts o $ reset

          puts o bold
          puts o " H "
          puts o $ reset

          puts o $ color Blue
          putIvoryString o (constRef strH)
          puts o $ reset

    handler systemInit "rnInit" $ do
      callback $ const $ do
        ledSetup $ platformRedLED
        ledSetup $ platformGreenLED

    canSend <- state "canSend"

    coroutineHandler acc msgPer "sendMsg" $ do
      cmdE <- emitter cmd 1

      return $ CoroutineBody $ \ yield -> do
        ledOn $ platformRedLED

        forever $ noBreak $ do

          ledOn $ platformGreenLED
          (arr :: Ref ('Stack s) ('Array len ('Stored Uint8))) <- local $ izero

          total <- noReturn $ packCayenne arr [
              Temperature (lastSample ~> sample_th_temperature)
            , Humidity    (lastSample ~> sample_th_humidity)
            ]

          (y :: Ref ('Stack s) UARTBuffer) <- local $ izero
          refCopy (y ~> stringDataL) arr
          store (y ~> stringLengthL) (signCast total)

          refCopy dbg y
          emit cmdE (constRef y)
          -- wait for next periodic message
          _ <- yield
          return ()

    handler txdone "rnTXdone" $ do
      callback $ const $ do
        store canSend true
        ledOff $ platformGreenLED
