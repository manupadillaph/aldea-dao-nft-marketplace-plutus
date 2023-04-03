{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- HLINT ignore "Use camelCase" -}
--------------------------------------------------------------------------------
module Validators.MarketNFTV2.OnChain where
--------------------------------------------------------------------------------
import qualified Ledger (scriptHashAddress)
import qualified Ledger.Ada as LedgerAda
import qualified Ledger.Address as LedgerAddress (Address)
import qualified Ledger.Value as LedgerValue
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Scripts as UtilsScriptsV2
import qualified Plutus.V2.Ledger.Api as LedgerApiV2 
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2 
import qualified Plutus.V2.Ledger.Tx as LedgerTxV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                 as TxAssocMap
import PlutusTx.Prelude hiding (unless)
--------------------------------------------------------------------------------
import qualified Validators.MarketNFTV2.Types as T
import qualified Validators.MarketNFTV2.Helpers as Helpers
--------------------------------------------------------------------------------
{-# INLINEABLE mkValidator #-}
mkValidator :: LedgerApiV2.CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator protocolID_CS datumRaw redemerRaw ctxRaw =
    let 
        !redeemer = PlutusTx.unsafeFromBuiltinData @T.RedeemerMarket redemerRaw
        !ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in if case redeemer of
            T.RedeemerBuyerBuyNFT ->
                let    
                    !inputsRef_WithDatum = Helpers.getReferenceInputsWithDatum @T.DatumProtocol ctx
                    !inputs_WithDatum = Helpers.getInputsWithDatum @T.DatumMarket ctx
                    !inputsRef_TxOut_Values_And_Datums = [(LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputsRef_WithDatum]
                    !inputs_TxOut_Values_And_Datums = [(LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum]
                in 
                    validateBuyerBuyNFT protocolID_CS ctx inputsRef_TxOut_Values_And_Datums inputs_TxOut_Values_And_Datums 
           
            T.RedeemerSellerGetBackNFT -> 
                let 
                    !datum = PlutusTx.unsafeFromBuiltinData @T.DatumMarket datumRaw
                in 
                    validateSellerGetBack datum ctx
            then ()
            else error ()

--------------------------------------------------------------------------------

{-# INLINEABLE validateBuyerBuyNFT #-}
validateBuyerBuyNFT :: LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> [(LedgerApiV2.Value, T.DatumProtocol)] -> [T.TxOut_Value_And_Datum] -> Bool
validateBuyerBuyNFT protocolID_CS ctx inputsRef_TxOut_Values_And_Datums inputs_TxOut_Values_And_Datums =
    traceIfFalse "ALDEA Market NFT: Valores incorrectos" checkPaymentInOutputs -- &&
    -- traceIfFalse "ALDEA Market NFT: Comisiones incorrectas!" checkComissions
  where
    ------------------
    info :: LedgerContextsV2.TxInfo
    !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
    !protocolID_AC = LedgerValue.AssetClass (protocolID_CS, T.protocolID_TN)
    ------------------
    !inputRef_TxOut_Value_And_ValidatorDatum =
        case Helpers.getTxOut_Value_And_SomeDatum protocolID_AC inputsRef_TxOut_Values_And_Datums of
            Nothing -> traceError "IRDP"
            Just x  -> x   
    ------------------
    !inputs_TxOuts_Values_And_ValidatorDatums =
        case inputs_TxOut_Values_And_Datums of
            []  -> traceError "IDMS"
            x   -> x
    ------------------
    !datumRef_In = Helpers.getTxOut_Datum inputRef_TxOut_Value_And_ValidatorDatum
    ------------------
    !aldeaPkh = T.aldeaPkh datumRef_In
    !aldeaPct = T.aldeaPct datumRef_In
    !protocolPkh = T.protocolPkh datumRef_In
    !protocolPct = T.protocolPct datumRef_In
    ------------------
    joinSameSeller :: [(LedgerApiV2.PubKeyHash, LedgerApiV2.Value)] -> [(LedgerApiV2.PubKeyHash, LedgerApiV2.Value, LedgerApiV2.Value)]
    joinSameSeller list = joinSameSellerHelper [] list
        where 
        ------------------
            joinSameSellerHelper :: [(LedgerApiV2.PubKeyHash, LedgerApiV2.Value, LedgerApiV2.Value)] -> [(LedgerApiV2.PubKeyHash, LedgerApiV2.Value)] -> [(LedgerApiV2.PubKeyHash, LedgerApiV2.Value, LedgerApiV2.Value)]
            joinSameSellerHelper seen [] = seen
            joinSameSellerHelper seen ((seller, value_For_Seller):xs) =
                let
                    !seller' = find (\(m, _, _) -> m == seller) seen
                in
                    case seller' of
                        Nothing -> 
                            let 
                                !value_For_Seller_Real = LedgerContextsV2.valuePaidTo info seller
                                !elemet = (seller, value_For_Seller, value_For_Seller_Real)
                            in 
                                joinSameSellerHelper (elemet:seen) xs
                        Just (_, v1, v2) -> 
                            let 
                                !elemet = (seller, v1 <> value_For_Seller, v2)
                                !seen_filter = filter (\(m', _, _) -> m' /= seller) seen
                            in
                                joinSameSellerHelper (elemet:seen_filter) xs
    ------------------
    !values_For_Each_Seller_And_Comissions = [ 
        let 
            !datum_In = Helpers.getTxOut_Datum input_TxOut_Value_And_ValidatorDatum
            ------------------
            !value_In_datum_In = Helpers.getTxOut_Value input_TxOut_Value_And_ValidatorDatum
            ------------------
            !value_minAda = LedgerAda.toValue $ LedgerAda.fromValue value_In_datum_In
            ------------------
            !seller = T.dSeller datum_In
            !priceADA' = T.dPriceADA datum_In
            !priceALDEA' = T.dPriceALDEA datum_In
            ------------------
        in
            case priceADA' of
                Nothing -> 
                    case priceALDEA' of
                        Nothing -> 
                            ((seller, value_minAda), (T.minComission, T.minComission))
                        Just priceALDEA ->
                            let 
                                !aldea_AC = LedgerValue.AssetClass (T.aldea_CS, T.aldea_TN)
                                !value_For_Seller = LedgerValue.assetClassValue aldea_AC priceALDEA <> value_minAda
                            in 
                                ((seller, value_For_Seller), (T.minComission, T.minComission))
                Just priceADA -> 
                    let 
                        !comissionsAldea' = aldeaPct * priceADA `divide` (100 * 100) -- usando puntos basicos para no perder precision
                        !comissionsAldea = if comissionsAldea' < T.minComission then T.minComission else comissionsAldea -- minimo 1 ADA de comisiones para Aldea
                        !comissionsProtocol' = protocolPct * priceADA `divide` (100 * 100) -- usando puntos basicos para no perder precision
                        !comissionsProtocol = if comissionsProtocol' < T.minComission then T.minComission else comissionsProtocol -- minimo 1 ADA de comisiones para Protocol
                        !price_For_Seller = priceADA - ((comissionsAldea + comissionsProtocol) `divide` 2) -- la mitad de las comisiones las paga el comprador y la otra mitad el vendedor
                        !value_For_Seller = LedgerAda.lovelaceValueOf price_For_Seller <> value_minAda
                    in
                        ((seller, value_For_Seller), (comissionsAldea, comissionsProtocol))
        | input_TxOut_Value_And_ValidatorDatum <- inputs_TxOuts_Values_And_ValidatorDatums
        ]
    ------------------
    -- sum all comissions for Aldea and Protocol
    !comissions = foldl (\(a1, a2) (b1, b2) -> (a1 + b1, a2 + b2)) (0, 0) (map snd values_For_Each_Seller_And_Comissions)
    ------------------
    !forAldea = LedgerAda.lovelaceValueOf $ fst comissions
    !forProtocol = LedgerAda.lovelaceValueOf $ snd comissions
    ------------------
    !values_For_Aldea = (aldeaPkh, forAldea)
    !values_For_Protocol = (protocolPkh, forProtocol)
    ------------------
    values_For_Each_Seller :: [(LedgerApiV2.PubKeyHash, LedgerApiV2.Value)]
    !values_For_Each_Seller = map fst values_For_Each_Seller_And_Comissions ++ [values_For_Aldea, values_For_Protocol]
    ------------------
    !values_For_Each_Seller_Accumulated = joinSameSeller values_For_Each_Seller
    ------------------
    -- checkComissions :: Bool
    -- !checkComissions =
    --     let
    --         !value_For_Aldea = LedgerContextsV2.valuePaidTo info aldeaPkh
    --         !value_For_Protocol = LedgerContextsV2.valuePaidTo info protocolPkh
    --         !comissions = Helpers.getComissions inputs_TxOuts_Values_And_ValidatorDatums
    --     in
    --         comissions == comissions'
    ------------------
    checkPaymentInOutputs :: Bool
    !checkPaymentInOutputs =  all (\(_, v1, v2) -> Helpers.valueIncludesValue v2 v1) values_For_Each_Seller_Accumulated

--------------------------------------------------------------------------------

{-# INLINEABLE validateSellerGetBack #-}
validateSellerGetBack :: T.DatumMarket -> LedgerContextsV2.ScriptContext -> Bool
validateSellerGetBack datum ctx =
  traceIfFalse "ALDEA Market NFT: Solo el vendedor puede recuperar su NFT" signedBySeller
  where
    info :: LedgerContextsV2.TxInfo
    !info = LedgerContextsV2.scriptContextTxInfo ctx

    signedBySeller :: Bool
    !signedBySeller = LedgerContextsV2.txSignedBy info $ T.dSeller datum

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-# INLINEABLE mkValidatorProtocol #-}
mkValidatorProtocol :: LedgerApiV2.CurrencySymbol -> [T.WalletPaymentPKH] -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorProtocol protocolID_CS pkhAdmins datumRaw redemerRaw ctxRaw =
    let 
        -- !datum = PlutusTx.unsafeFromBuiltinData @T.DatumProtocol datumRaw
        !redeemer = PlutusTx.unsafeFromBuiltinData @T.RedeemerProtocol redemerRaw
        !ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    in if case redeemer of
            T.RedeemerUpdateDatum->
                let    
                    !inputs_WithDatum = Helpers.getInputsWithDatum @T.DatumProtocol ctx
                    !outputs_WithDatum = Helpers.getOutputsWithDatum @T.DatumProtocol ctx
                    !inputs_TxOut_Values_And_Datums = [(LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- inputs_WithDatum]
                    !outputs_TxOut_Values_And_Datums = [(LedgerApiV2.txOutValue txtout, dat) | (txtout, dat) <- outputs_WithDatum]
                in 
                    validateUpdate pkhAdmins protocolID_CS ctx inputs_TxOut_Values_And_Datums outputs_TxOut_Values_And_Datums
            then ()
            else error ()

--------------------------------------------------------------------------------

{-# INLINEABLE validateUpdate #-}
validateUpdate :: [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> LedgerContextsV2.ScriptContext -> [(LedgerApiV2.Value, T.DatumProtocol)] -> [(LedgerApiV2.Value, T.DatumProtocol)] ->Bool
validateUpdate pkhAdmins protocolID_CS ctx inputs_TxOut_Values_And_DatumsProtocol outputs_TxOut_Values_And_DatumsProtocol =
    traceIfFalse "ALDEA Market NFT: Update Incorrecto!" checkUpdate 
    &&
    traceIfFalse "ALDEA Market NFT: Firma invalida!" checkSignature 
  where
    ------------------
    info :: LedgerContextsV2.TxInfo
    !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
    protocolID_AC = LedgerValue.AssetClass (protocolID_CS, T.protocolID_TN)
    ------------------
    !input_TxOut_Value_And_DatumProtocol =
        case Helpers.getTxOut_Value_And_SomeDatum protocolID_AC inputs_TxOut_Values_And_DatumsProtocol of
            Nothing -> traceError "IPD"
            Just x  -> x     
    ------------------
    !output_TxOut_Value_And_DatumProtocol =
        case Helpers.getTxOut_Value_And_SomeDatum protocolID_AC outputs_TxOut_Values_And_DatumsProtocol of
            Nothing -> traceError "OPD"
            Just x  -> x   
    ------------------
    checkUpdate :: Bool
    !checkUpdate =
        let
            !value_In = Helpers.getTxOut_Value input_TxOut_Value_And_DatumProtocol
            !value_Out = Helpers.getTxOut_Value output_TxOut_Value_And_DatumProtocol
        in
            Helpers.valueEqualsValue value_In value_Out
    ------------------
    checkSignature :: Bool
    checkSignature = Helpers.signedByPubKeyHash pkhAdmins info

--------------------------------------------------------------------------------

{-# INLINEABLE codeValidator #-}
codeValidator :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.Validator
codeValidator protocolID_CS = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidator protocolID_CS

{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: LedgerApiV2.CurrencySymbol ->  Plutonomy.Validator
plutonomyValidator protocolID_CS =
  Plutonomy.mkValidatorScript $ 
    $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode protocolID_CS

--------------------------------------------------------------------------------

{-# INLINEABLE codeValidatorProtocol #-}
codeValidatorProtocol :: LedgerApiV2.CurrencySymbol -> [T.WalletPaymentPKH] -> LedgerApiV2.Validator
codeValidatorProtocol protocolID_CS pkhAdmins =
  Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ plutonomyValidatorProtocol protocolID_CS pkhAdmins

{-# INLINEABLE plutonomyValidatorProtocol #-}
plutonomyValidatorProtocol :: LedgerApiV2.CurrencySymbol -> [T.WalletPaymentPKH] -> Plutonomy.Validator
plutonomyValidatorProtocol protocolID_CS pkhAdmins =
  Plutonomy.mkValidatorScript $ 
    $$(PlutusTx.compile [||mkValidatorProtocol||])
        `PlutusTx.applyCode` PlutusTx.liftCode protocolID_CS
        `PlutusTx.applyCode` PlutusTx.liftCode pkhAdmins

--------------------------------------------------------------------------------

