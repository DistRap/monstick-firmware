{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Monstick.Types where

import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib
import Ivory.Tower.Drivers.Net.LoRa

[ivory| string struct UARTBuffer 64 |]

uartTypes :: Module
uartTypes = package "uartTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

monstickTowerDeps :: Tower e ()
monstickTowerDeps = do
  towerDepends uartTypes
  towerModule uartTypes

type Channel = Int

data (Time t) => WANRegion t = WANRegion {
    wanRegionDefaultChannels  :: [ Channel ]
  , wanRegionRX2Frequency     :: Int
  , wanRegionRX2DataRate      :: DataRate
  , wanRegionReceiveDelay1    :: t
  , wanRegionReceiveDelay2    :: t
  , wanRegionJoinAcceptDelay1 :: t
  , wanRegionJoinAcceptDelay2 :: t
  , wanRegionMaxFCntGap       :: Int
  , wanRegionADRAckLimit      :: Int
  , wanRegionADRAckDelay      :: Int
  , wanRegionAckTimeout       :: t
  , wanRegionDataRates        :: [ DataRate ]
  }

mhz = round . (*100_000)

-- | EU863-870 channel plan and regional defaults
eu868 = WANRegion {
    wanRegionDefaultChannels  = [ mhz 868.1, mhz 868.3, mhz 868.5 ]
  , wanRegionRX2Frequency     = mhz 869.525
  , wanRegionRX2DataRate      = dataRate dataRates 0
  , wanRegionReceiveDelay1    = Seconds 1
  , wanRegionReceiveDelay2    = Seconds 2 -- must be wanRegionReceiveDelay1 + 1 s
  , wanRegionJoinAcceptDelay1 = Seconds 5
  , wanRegionJoinAcceptDelay2 = Seconds 6
  , wanRegionMaxFCntGap       = 16384
  , wanRegionADRAckLimit      = 64
  , wanRegionADRAckDelay      = 32
  , wanRegionAckTimeout       = Seconds 2 -- +/- 1 s (random delay between 1 and 3 seconds)
  , wanRegionDataRates        = dataRates
  }
  where
    dataRates = [
        DataRate 0 sf12 bw125
      , DataRate 1 sf11 bw125
      , DataRate 2 sf10 bw125
      , DataRate 3 sf9  bw125
      , DataRate 4 sf8  bw125
      , DataRate 5 sf7  bw125
      , DataRate 6 sf7  bw250
      ]
    bands = [
        Band "g" (mhz 863.0) (mhz 868.0)   1 -- 1%
      , Band "g1" (mhz 868.0) (mhz 868.6)  1
      , Band "g2" (mhz 868.7) (mhz 869.2)  0.1
      , Band "g3" (mhz 869.4) (mhz 869.65) 10
      , Band "g4" (mhz 869.7) (mhz 870.0)  1
      ]


dataRate drs n = case filter (\(DataRate d _ _) -> d == n) drs of
  [one] -> one
  _ -> error $ "No DataRate with ID found " ++ show n

-- TTN uses non standard RX2 window data rate 3 (SF9, BW125)
eu868TTN = eu868 { wanRegionRX2DataRate = dataRate (wanRegionDataRates eu868) 3 }

data Band = Band {
    bandName      :: String
  , bandStartFreq :: Int
  , bandEndFreq   :: Int
  , bandDutyCycle :: Float
  } deriving (Eq, Show, Ord)

data DataRate = DataRate Int SpreadingFactor Bandwidth

dataRateToSpreadingFactor
  :: GetAlloc eff ~ 'Scope s
  => [DataRate]
  -> Uint8
  -> Ivory eff SpreadingFactor
dataRateToSpreadingFactor drlist dr = cond
  $ map (\(DataRate d sf _bw) -> dr ==? fromIntegral d ==> return sf) drlist

dataRateToBandwidth
  :: GetAlloc eff ~ 'Scope s
  => [DataRate]
  -> Uint8
  -> Ivory eff Bandwidth
dataRateToBandwidth drlist dr = cond
  $ map (\(DataRate d _sf bw) -> dr ==? fromIntegral d ==> return bw) drlist
