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
module Validators.MarketNFTV2.Helpers where
--------------------------------------------------------------------------------
-- import qualified Ledger
-- import qualified Ledger.Ada as LedgerAda
-- import qualified Ledger.Address as LedgerAddress (Address)
-- import qualified Plutonomy
-- import qualified Plutus.Script.Utils.V2.Scripts as UtilsScriptsV2
import qualified Plutus.V2.Ledger.Api as LedgerApiV2 
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V2.Ledger.Tx as LedgerTxV2 (txOutDatum , OutputDatum (..)) 
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                 as TxAssocMap
import PlutusTx.Prelude hiding (unless)
--------------------------------------------------------------------------------
import qualified Validators.MarketNFTV2.Types as T
import qualified Ledger.Value as LedgerValue
--------------------------------------------------------------------------------
{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just valueInfo)   = valueInfo
fromJust Nothing            = traceError "JN"
--------------------------------------------------------------------------------
{-# INLINABLE getDatumFromTxOut #-}
getDatumFromTxOut :: forall a. PlutusTx.UnsafeFromData a => LedgerApiV2.TxOut -> LedgerContextsV2.ScriptContext -> Maybe a
getDatumFromTxOut txtout ctx =
    let
        findDatum LedgerTxV2.NoOutputDatum        = Nothing
        findDatum (LedgerTxV2.OutputDatumHash dh) = LedgerContextsV2.findDatum dh (LedgerContextsV2.scriptContextTxInfo ctx)
        findDatum (LedgerTxV2.OutputDatum d)      = Just d -- LedgerContextsV2.findDatumHash d (LedgerContextsV2.scriptContextTxInfo ctx) >> 
    in
        case findDatum $ LedgerTxV2.txOutDatum txtout of
            Nothing -> Nothing
            Just x -> Just $ LedgerApiV2.unsafeFromBuiltinData $ LedgerApiV2.getDatum x
--------------------------------------------------------------------------------
{-# INLINABLE getInputsWithDatum #-}
getInputsWithDatum :: forall a. PlutusTx.UnsafeFromData a => LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TxOut, a)]
getInputsWithDatum ctx =
    let
        !txOuts = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
        !txOutReftxOutsAndDatums = [  (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts]
        !txOutReftxOutsAndJustDatums = [  (txOut, fromJust dat) | (txOut, dat) <- txOutReftxOutsAndDatums, isJust dat ]
    in
        txOutReftxOutsAndJustDatums
--------------------------------------------------------------------------------
{-# INLINABLE getReferenceInputsWithDatum #-}
getReferenceInputsWithDatum :: forall a. PlutusTx.UnsafeFromData a => LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TxOut, a)]
getReferenceInputsWithDatum !ctx =
    let 
        !txOutReftxOutsAndDatums =
            let
                !txOuts = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs (LedgerContextsV2.scriptContextTxInfo ctx)]
            in
                [(txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts]
    in
        [ (txtout, fromJust dat) | (txtout, dat) <- txOutReftxOutsAndDatums, isJust dat ]
           
--------------------------------------------------------------------------------
{-# INLINABLE getOutputsWithDatum #-}
getOutputsWithDatum :: forall a. PlutusTx.UnsafeFromData a => LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TxOut, a)]
getOutputsWithDatum ctx =
    let
        !txOuts    = LedgerApiV2.txInfoOutputs (LedgerContextsV2.scriptContextTxInfo ctx)
        !txOutsAndDatums = [  (txOut, getDatumFromTxOut txOut ctx) | txOut <- txOuts ]
        !txOutsAndJustDatums = [  (txtout, fromJust dat) | (txtout, dat) <- txOutsAndDatums, isJust dat ]
    in
        txOutsAndJustDatums
--------------------------------------------------------------------------------
{-# INLINABLE getTxOut_Value_And_SomeDatum #-}
getTxOut_Value_And_SomeDatum :: LedgerValue.AssetClass -> [(LedgerApiV2.Value, dat)] -> Maybe (LedgerApiV2.Value, dat)
getTxOut_Value_And_SomeDatum !ac !txOuts_Value_And_Datum =
    let
        txIDInOut =
            let
                txIDInOutWithSomeDatum = [ (value, datum) | (value, datum) <- txOuts_Value_And_Datum, isToken_With_AC_InValue value ac]
            in
                txIDInOutWithSomeDatum
    in
        case txIDInOut of
            [x] -> Just x
            _   -> Nothing
--------------------------------------------------------------------------------
{-# INLINABLE isToken_With_AC_InValue #-}
isToken_With_AC_InValue :: LedgerApiV2.Value -> LedgerValue.AssetClass -> Bool
isToken_With_AC_InValue !value !ac = LedgerValue.assetClassValueOf value ac >= 1
--------------------------------------------------------------------------------
{-# INLINABLE getTxOut_Value #-}
getTxOut_Value :: (x, y) -> x
getTxOut_Value = fst

{-# INLINABLE getTxOut_Datum #-}
getTxOut_Datum :: (x, y) -> y
getTxOut_Datum = snd
--------------------------------------------------------------------------------
{-# INLINABLE listTNEqualsListTN #-}
listTNEqualsListTN :: [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
listTNEqualsListTN [] [] = True
listTNEqualsListTN [] ((_, !am2):(!xs2)) =
    am2 == 0 && listTNEqualsListTN [] xs2
listTNEqualsListTN ((_, !am1):(!xs1)) [] =
    am1 == 0 && listTNEqualsListTN xs1 []
listTNEqualsListTN ((!tn1, !am1):(!xs1)) ((!tn2, !am2):(!xs2))
    | am1 == 0 =
        listTNEqualsListTN xs1 ((tn2, am2):xs2)
    | am2 == 0 =
        listTNEqualsListTN ((tn1, am1):xs1) xs2
    | tn1 == tn2 && am1 == am2 =
        listTNEqualsListTN xs1 xs2
    | otherwise =
        let
            listTNEqualsListTN' :: (LedgerApiV2.TokenName, Integer) -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> [(LedgerApiV2.TokenName, Integer)] -> Bool
            listTNEqualsListTN' (!tn1', !am1') !xs1' !xs2' ((!tn2', !am2'):(!xs3'))
                | am2' == 0                     = listTNEqualsListTN' (tn1', am1') xs1' xs2' xs3'
                | tn1' == tn2' && am1' == am2'  = listTNEqualsListTN xs1' (xs2'++xs3')
                | otherwise                     = listTNEqualsListTN' (tn1', am1') xs1' ((tn2', am2'):xs2') xs3'
            listTNEqualsListTN' _ _ _ _ = False
        in
            listTNEqualsListTN' (tn1, am1) xs1 [(tn2, am2)] xs2

---------------------------------------------------
{-# INLINABLE listCSEqualsListCS #-}
listCSEqualsListCS :: [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
listCSEqualsListCS [] [] = True
listCSEqualsListCS ((_, !mp1):(!xs1)) [] =
        let
            !listTN1 = TxAssocMap.toList mp1
        in
            listTNEqualsListTN listTN1 [] && listCSEqualsListCS xs1 []
listCSEqualsListCS [] ((_, !mp2):(!xs2)) =
        let
            !listTN2 = TxAssocMap.toList mp2
        in
            listTNEqualsListTN [] listTN2 && listCSEqualsListCS [] xs2
listCSEqualsListCS ((!cs1, !mp1):(!xs1)) ((!cs2, !mp2):(!xs2))
    | cs1 == cs2 =
        let
            !listTN1 = TxAssocMap.toList mp1
            !listTN2 = TxAssocMap.toList mp2
        in
            listTNEqualsListTN listTN1 listTN2 && listCSEqualsListCS xs1 xs2
    | otherwise =
        let
            listCSEqualsListCS' :: (LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer) -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> [(LedgerApiV2.CurrencySymbol, TxAssocMap.Map LedgerApiV2.TokenName Integer)] -> Bool
            listCSEqualsListCS' (!_, !mp1') !xs1' !xs2' [] =
                let
                    !listTN1 = TxAssocMap.toList mp1'
                in
                    listTNEqualsListTN listTN1 [] && listCSEqualsListCS xs1' xs2'
            listCSEqualsListCS' (!cs1', !mp1') !xs1' !xs2' ((!cs2', !mp2'):xs3') =
                if cs1' == cs2' then
                    let
                        !listTN1 = TxAssocMap.toList mp1'
                        !listTN2 = TxAssocMap.toList mp2'
                    in
                        listTNEqualsListTN listTN1 listTN2 && listCSEqualsListCS xs1' (xs2'++xs3')
                else
                    listCSEqualsListCS' (cs1', mp1') xs1' ((cs2', mp2'):xs2') xs3'
        in
            listCSEqualsListCS' (cs1, mp1) xs1 [(cs2, mp2)] xs2
---------------------------------------------------
{-# INLINABLE flattenValue #-}
flattenValue :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValue (LedgerValue.Value mp) =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp')  <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4 ]
    in
        f3
--------------------------------------------------------------------------------
{-# INLINABLE valueIncludesValue #-}
valueIncludesValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueIncludesValue value valueToFind  =
    let
        !valueToFind' = flattenValue valueToFind
    in
        all (\(cs, tn, amount) -> 
                let 
                    !ac = LedgerValue.AssetClass (cs, tn)
                in LedgerValue.assetClassValueOf value ac >= amount
            ) valueToFind'
--------------------------------------------------------------------------------
{-# INLINABLE valueEqualsValue #-}
valueEqualsValue :: LedgerApiV2.Value -> LedgerApiV2.Value -> Bool
valueEqualsValue (LedgerValue.Value !mp1) (LedgerValue.Value !mp2) =
    let
        !listCS1 = TxAssocMap.toList mp1
        !listCS2 = TxAssocMap.toList mp2
    in
        listCS1 `listCSEqualsListCS` listCS2    
--------------------------------------------------------------------------------
{-# INLINABLE signedByPubKeyHash #-}
signedByPubKeyHash :: [T.WalletPaymentPKH] -> LedgerContextsV2.TxInfo -> Bool
signedByPubKeyHash !pubKeyHashes !info =
    any (LedgerContextsV2.txSignedBy info) pubKeyHashes
--------------------------------------------------------------------------------
{-# INLINABLE getOwnMintedTokenNameAndAmt  #-}
getOwnMintedTokenNameAndAmt :: LedgerContextsV2.ScriptContext -> [(LedgerApiV2.TokenName, Integer)]
getOwnMintedTokenNameAndAmt !ctx =
    let
        !cs = LedgerContextsV2.ownCurrencySymbol ctx

        info :: LedgerContextsV2.TxInfo
        !info = LedgerContextsV2.scriptContextTxInfo ctx

        !flatten = TxAssocMap.lookup cs (LedgerApiV2.getValue $ LedgerApiV2.txInfoMint info)
    in
        TxAssocMap.toList $ fromJust flatten
--------------------------------------------------------------------------------
{-# INLINABLE validateMint_NFT_Own_CS_Any_TN #-}
validateMint_NFT_Own_CS_Any_TN :: LedgerContextsV2.ScriptContext -> Bool
validateMint_NFT_Own_CS_Any_TN !ctx  =
    traceIfFalse "MAMT" checkNFTMintedAmountV2  
    where
        !checkNFTMintedAmountV2 =
            case getOwnMintedTokenNameAndAmt ctx of
                []  -> False
                x   -> all (\(_, amt) -> amt == 1) x
--------------------------------------------------------------------------------
{-# INLINABLE hasInputUTxO #-}
hasInputUTxO :: LedgerApiV2.TxOutRef -> LedgerContextsV2.TxInfo -> Bool
hasInputUTxO !txOutRef !info = any (\i -> LedgerApiV2.txInInfoOutRef i == txOutRef) $ LedgerApiV2.txInfoInputs info
-------------------------------------------------------------------------------------------  
