
module Monstick.Platforms.Types where

import Ivory.Tower.Base (LED)

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.MCU
import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.I2C

import Ivory.Tower.Drivers.Net.RN2483

data Platform = Platform {
    platformMCU      :: Maybe NamedMCU
  , platformMCUName  :: String
  , platformClocks   :: ClockConfig
  , platformPin      :: GPIOPin
  , platformRedLED   :: LED
  , platformGreenLED :: LED
  , platformSPI      :: SPI
  , platformSPIPins  :: SPIPins
  , platformSPIDevs  :: [ SPIDevice ]
  , platformI2C      :: I2C
  , platformI2CPins  :: I2CPins
  , platformUART     :: UART
  , platformUARTPins :: UARTPins
  , platformCAN      :: CANConfig
  , platformRadio    :: Radio
  }

undef :: String -> a
undef part = error $ part ++ " is not available on this platform"

data CANConfig = CANConfig
  { canPeriph        :: CANPeriph
  , canPeriphFilters :: CANPeriphFilters
  , canRxPin         :: GPIOPin
  , canTxPin         :: GPIOPin
  }

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
