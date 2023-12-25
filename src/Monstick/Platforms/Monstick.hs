
module Monstick.Platforms.Monstick where


import Ivory.Tower
import Ivory.OS.FreeRTOS.Tower.STM32

import Hello.Tests.Platforms
import Hello.Tests.Platforms.Monstick

import Monstick.Platforms.Types (MikroBUS(..), Radio(..))

import Ivory.BSP.STM32L431

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.SPI
import Ivory.BSP.STM32.Peripheral.UART

import qualified Ivory.Tower.Base as Base
import Ivory.Tower.Drivers.Net.RN2483 (defaultRadioConfig)
import Ivory.Tower.Drivers.SPIDevice (genericSPIDev)

monstickRadio :: Radio
monstickRadio = Radio {
    radioUART        = usart1
  , radioUARTPins    = usart1Pins
  , radioConfig      = defaultRadioConfig
  , radioResetPin    = pinC12
  }

-- | Used by both Mikrobuses
usart3Pins :: UARTPins
usart3Pins = UARTPins
  { uartPinTx = pinC10
  , uartPinRx = pinC11
  }

-- | Closer to core (middle, CON1)
monstickMikrobus1 :: MikroBUS
monstickMikrobus1 = MikroBUS {
    mbUART     = usart3
  , mbUARTPins = usart3Pins
  , mbSPI      = spi2
  , mbSPIPins  = spiPins
  , mbSPICS    = pinB12
  , mbI2C      = i2c3
  , mbI2CPins  = i2c3Pins
  , mbADC      = pinC4 -- ADC2,GPIO12
  , mbISR      = pinC8 -- GPIO1
  , mbPWM      = pinA5 -- GPIO8
  , mbRST      = pinC5
  }

-- | Edge (CON2)
monstickMikrobus2 :: MikroBUS
monstickMikrobus2 = MikroBUS {
    mbUART     = usart3
  , mbUARTPins = usart3Pins
  , mbSPI      = spi2
  , mbSPIPins  = spiPins
  , mbSPICS    = pinB9
  , mbI2C      = i2c1
  , mbI2CPins  = i2c1Pins
  , mbADC      = pinA4 -- ADC3,GPIO13
  , mbISR      = pinC7 -- GPIO2
  , mbPWM      = pinA8 -- GPIO0
  , mbRST      = pinB0
  }

mikrobusSPIDev :: String -> MikroBUS -> SPIDevice
mikrobusSPIDev name mb = genericSPIDev name (mbSPI mb) (mbSPICS mb)

mikrobus_rfm1 :: SPIDevice
mikrobus_rfm1 = mikrobusSPIDev "rfm95w1" monstickMikrobus1

mikrobus_rfm2 :: SPIDevice
mikrobus_rfm2 = mikrobusSPIDev "rfm95w2" monstickMikrobus2
