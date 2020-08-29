
module Monstick.Platforms.Types where

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.I2C

import Ivory.Tower.Drivers.Net.RN2483

data Radio = Radio
  { radioUART     :: UART
  , radioUARTPins :: UARTPins
  , radioConfig   :: RadioConfig
  , radioResetPin :: GPIOPin
  }

data MikroBUS = MikroBUS {
    mbUART      :: UART
  , mbUARTPins  :: UARTPins
  , mbSPI       :: SPI
  , mbSPICS     :: GPIOPin
  , mbSPIPins   :: SPIPins
  , mbI2C       :: I2C
  , mbI2CPins   :: I2CPins
  , mbADC       :: GPIOPin
  , mbISR       :: GPIOPin
  , mbPWM       :: GPIOPin
  , mbRST       :: GPIOPin
  }
