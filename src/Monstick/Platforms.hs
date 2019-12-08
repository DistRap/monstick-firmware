{-# LANGUAGE RecordWildCards #-}
module Monstick.Platforms (
    platformParser
  , getPlatform
  , buildMonstickApp
  , module Monstick.Platforms.Monstick
  , module Monstick.Platforms.Types
  ) where

import Monstick.Platforms.Types
import Monstick.Platforms.Monstick (monstick)

import Data.Char (toLower)
import Ivory.Tower
import Ivory.Tower.Options
import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32
import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Drivers.Net.RN2483

platformParser :: Platform -> ConfigParser Platform
platformParser defPlatform = do
  rc <- withDefault radioConfigParser defaultRadioConfig
  mp <- optional $ subsection "args" $ subsection "platform" string
  plat <- case fmap (map toLower) mp of
    Nothing         -> return defPlatform
    Just "monstick" -> return monstick
    Just x          -> fail ("no such platform " ++ x)

  return $ plat { platformRadio = (platformRadio plat) { radioConfig = rc } }

-- allows overriding platform, default platforms MCU and few of its params
-- via default.conf
getPlatform :: Platform -> TOpts -> IO Platform
getPlatform defPlatform topts = do
    -- first platformParser
    p <- getConfig topts (platformParser defPlatform)
    -- then possible mcu override with default mcu from platform
    mcuName <- getConfig topts (mcuNameParser (platformMCUName p))

    -- lookup mcu and set in platformMCU
    nmcu <- matchMCU mcuName

    -- mcu options (ram, flash, splitmem..)
    nmcuCfg <- getConfig topts (mcuConfigParser nmcu)

    return $ p { platformMCU = Just nmcuCfg }

platformToConfig :: Platform -> STM32Config
platformToConfig Platform{platformMCU = (Just mcu), platformClocks = cc} = STM32Config mcu cc
platformToConfig _ = error "platformMCU not initialized"

-- for our monstick applications
-- with type
-- app :: (e -> ClockConfig) -> (e -> Platform) -> Tower e ()
-- we can define this helper
buildMonstickApp :: Platform
       -> ((Platform -> ClockConfig) -> (Platform -> Platform) -> Tower Platform ())
       -> IO ()
buildMonstickApp def twrapp = compileTowerSTM32FreeRTOS
  platformToConfig (getPlatform def) $ twrapp platformClocks id
