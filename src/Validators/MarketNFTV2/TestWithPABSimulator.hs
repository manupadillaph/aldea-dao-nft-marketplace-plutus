{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE NumericUnderscores         #-}
--------------------------------------------------------------------------------
module Validators.MarketNFTV2.TestWithPABSimulator where
--------------------------------------------------------------------------------
import qualified Control.Monad                       as Monad (void)
--import qualified Data.Default                      as DataDefault (def) 
--import qualified Data.Map                          as DataMap
--import qualified Ledger.Ada                        as LedgerAda 
--import qualified Ledger.TimeSlot                   as LedgerTimeSlot (slotToEndPOSIXTime)    
--import qualified Playground.Contract               as PlaygroundContract (IO) --, ensureKnownCurrencies, printSchemas, stage, printJson
import qualified Plutus.Trace.Emulator               as TraceEmulator
--import qualified Plutus.V1.Ledger.Value            as LedgerValueV1
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
--import qualified Wallet.Emulator.Wallet            as WalletEmulator
--------------------------------------------------------------------------------
--import qualified Validators.MarketNFTV2.OffChain    as OffChain
--------------------------------------------------------------------------------
testWithPABSimulator :: P.IO ()
testWithPABSimulator = TraceEmulator.runEmulatorTraceIO $ do

    Monad.void $ TraceEmulator.waitNSlots 1