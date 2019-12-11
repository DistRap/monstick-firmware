{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Monstick.Types where

import Ivory.Language
import Ivory.Tower

[ivory| string struct UARTBuffer 64 |]

uartTypes :: Module
uartTypes = package "uartTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

monstickTowerDeps :: Tower e ()
monstickTowerDeps = do
  towerDepends uartTypes
  towerModule uartTypes
