{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monstick.Test.RadioCommand where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Base

import Ivory.Tower.Drivers.Net.RN2483

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

  -- pretty istream from RN2483
  istreamRadio' <- replaceChar '\r' '\n' istreamRadio
                     >>= dropEvery 2 (`isChar` '\n')
  -- UART echo
  fwd istream ostream

  -- screen only sends \r but radio needs \r\n
  istreamLF <- appendCharAfter '\r' '\n' istream
  uartBridge ostream istreamLF ostreamRadio istreamRadio'

  -- prepend RN2483 commands with >
  dbgstream <- dbgPrepend '\n'
    (clearLineCode ++ reset ++ "\r> ") ostream

  -- create merged input channel that feeds both
  -- ostream' and dbgstream
  merged <- ostreamRadio `mergeInputs` dbgstream

  (acc, cmdMode, BackpressureTransmit cmd txdone) <- rn2483Tower
    (radioConfig $ platformRadio)
    merged istreamRadio systemInit
    (radioResetPin $ platformRadio)
    (Proxy :: Proxy UARTBuffer)

  msgPer <- period (Milliseconds 1000)

  monitor "rnleds" $ do
    monitorModuleDef $ hw_moduledef

    handler systemInit "rnInit" $ do
      cmdModeE <- emitter cmdMode 1
      callback $ const $ do
        emitV cmdModeE modeRaw
        ledSetup $ platformRedLED
        ledSetup $ platformGreenLED

    coroutineHandler acc msgPer "sendMsg" $ do
      cmdE <- emitter cmd 1

      return $ CoroutineBody $ \ yield -> do
        ledOn $ platformRedLED

        x <- local $ stringInit "mac get dr"
        emit cmdE (constRef x)

        forever $ noBreak $ do
          ledOn $ platformGreenLED
          -- do nothing
          -- wait for next periodic message
          _ <- yield
          return ()

    handler txdone "rnTXdone" $ do
      callback $ const $ do
        ledOff $ platformGreenLED
