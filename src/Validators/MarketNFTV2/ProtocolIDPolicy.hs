{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{- HLINT ignore "Use camelCase" -}
------------------------------------------------------------------------------------------
module Validators.MarketNFTV2.ProtocolIDPolicy where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api                                       as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                                  as LedgerContextsV2 (ScriptContext, TxInfo, scriptContextTxInfo) 
import qualified PlutusTx   
import           PlutusTx.Prelude                                           
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified Validators.MarketNFTV2.Helpers  as Helpers
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
{-# INLINABLE mkPolicyProtocolID #-}
mkPolicyProtocolID :: LedgerApiV2.TxOutRef -> BuiltinData -> BuiltinData -> ()
mkPolicyProtocolID !txOutRef _ !ctxRaw =
    let
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        checkMinting:: Bool
        !checkMinting =
            case Helpers.getOwnMintedTokenNameAndAmt ctx  of
                []  -> False
                x   -> all (\(_, amt) -> amt > 0) x
    in
        if
            (checkMinting &&
                traceIfFalse "UTXO" (Helpers.hasInputUTxO txOutRef info) && 
                Helpers.validateMint_NFT_Own_CS_Any_TN ctx
            ) ||
            not checkMinting
        then ()
        else error ()
--------------------------------------------------------------------------------

{-# INLINEABLE policy_ProtocolID #-}
policy_ProtocolID :: LedgerApiV2.TxOutRef -> LedgerApiV2.MintingPolicy
policy_ProtocolID txOutRef  = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy_ProtocolID txOutRef

{-# INLINEABLE original_policy_ProtocolID #-}
original_policy_ProtocolID :: LedgerApiV2.TxOutRef -> Plutonomy.MintingPolicy
original_policy_ProtocolID txOutRef  =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicyProtocolID ||])
    `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

--------------------------------------------------------------------------------
