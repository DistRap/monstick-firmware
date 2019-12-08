
module Monstick.VT100 (
    bold
  , faint
  , normal
  , color
  , bgColor
  , reset
  , module System.Console.ANSI.Codes
  , module System.Console.ANSI.Types
  ) where

import System.Console.ANSI.Types
import System.Console.ANSI.Codes

bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

faint :: String
faint = setSGRCode [SetConsoleIntensity FaintIntensity]

normal :: String
normal = setSGRCode [SetConsoleIntensity NormalIntensity]

color :: Color -> String
color c = setSGRCode [SetColor Foreground Vivid c]

bgColor :: Color -> String
bgColor c = setSGRCode [SetColor Background Vivid c]

reset :: String
reset = setSGRCode [Reset]
