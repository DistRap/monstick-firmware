{-# LANGUAGE RecordWildCards #-}
module Monstick.Platforms (
    monstick
  , buildMonstickApp
  , MonstickPlatform(..)
  , Platform(..)
  , module Monstick.Platforms.Monstick
  , module Monstick.Platforms.Types
  ) where

import Ivory.Tower
import Ivory.BSP.STM32.ClockConfig

import Hello.Tests.Platforms (Platform(..), buildWrappedApp)
import qualified Hello.Tests.Platforms as Hello

import Monstick.Platforms.Types
import Monstick.Platforms.Monstick

data MonstickPlatform = MonstickPlatform {
    basePlatform :: Platform
  , radio        :: Radio
  , mikrobus1    :: MikroBUS
  , mikrobus2    :: MikroBUS
  }

monstick :: MonstickPlatform
monstick = MonstickPlatform {
    basePlatform = Hello.monstick
  , radio        = monstickRadio
  , mikrobus1    = monstickMikrobus1
  , mikrobus2    = monstickMikrobus2
  }

buildMonstickApp :: MonstickPlatform
                 -> (
                         (MonstickPlatform -> ClockConfig)
                      -> (MonstickPlatform -> MonstickPlatform)
                      -> Tower MonstickPlatform ()
                    )
                 -> IO ()
buildMonstickApp = buildWrappedApp unWrap wrap
  where
    unWrap :: MonstickPlatform -> Platform
    unWrap = basePlatform

    wrap :: Platform -> MonstickPlatform
    wrap x = monstick { basePlatform = x }
