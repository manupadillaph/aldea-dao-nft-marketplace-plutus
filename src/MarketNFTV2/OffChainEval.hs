{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE GADTs              #-}
{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
------------------------------------------------------------------------------------------
module MarketNFTV2.OffChainEval where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Cardano.Api                                                    as CardanoApi
import qualified Cardano.Api.Shelley                                            as CardanoApiShelley
import qualified Cardano.Binary                                                 as CardanoBinary ( ToCBOR,serializeEncoding)
import qualified Cardano.Ledger.Core                                            as CardanoLedgerCore
-- import qualified Cardano.Ledger.Alonzo                                       as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx                                       as AlonzoTx
import qualified Cardano.Ledger.Alonzo.TxWitness                                as AlonzoTxWitness
-- import qualified Cardano.Ledger.Alonzo.PParams                               as AlonzoPParams
-- import qualified Cardano.Ledger.Alonzo.PlutusScriptApi                       as AlonzoPlutusScriptApi
-- import qualified Cardano.Ledger.Alonzo.TxBody                                as AlonzoTxBody
-- import qualified Cardano.Ledger.Alonzo.TxInfo                                as AlonzoTxInfo
-- import qualified Cardano.Ledger.Alonzo.TxWitness                             as AlonzoTxWitness
import qualified Cardano.Ledger.Alonzo.Scripts                                  as AlonzoScripts
-- import qualified Cardano.Ledger.Babbage                                      as Babbage
import qualified Cardano.Ledger.Babbage.TxBody                                  as BabbageTxBody
import qualified Cardano.Ledger.Babbage.TxInfo                                  as BabbageTxInfo
-- import qualified Cardano.Ledger.Crypto                                       as LedgerCrypto
-- import qualified Cardano.Ledger.Mary.Value                                   as LedgerMaryValue
-- import qualified Cardano.Ledger.Shelley.API                                  as LedgerShelleyApi
-- import qualified Cardano.Ledger.Shelley.TxBody                               as LedgerShelleyTxBody 
-- import qualified Cardano.Node.Emulator.Chain                                    as CardanoNodeEmulatorChain
-- import qualified Cardano.Node.Emulator.Validation                               as CardanoNodeEmulatorValidation
-- import qualified Cardano.Node.Emulator.TimeSlot                                 as CardanoNodeEmulatorTimeSlot     
-- import qualified Cardano.Wallet.Shelley.Compatibility                        as CardanoWalletShelleyCompatibility
import qualified Control.Lens                                                   as ControlLens
-- import qualified Control.Lens.TH                                                   as ControlLensTH
-- import qualified Control.Monad.Error.Lens                                       as ControlMonadErrorLens
import qualified Data.ByteString.Lazy                                           as LBS
import qualified Data.Map                                                       as DataMap
-- import qualified Data.String                                                                        as DataString (IsString)
-- import qualified Data.Either                                                   as DataEither
-- import qualified Data.Maybe                                                  as DataMaybe
-- import qualified Data.SatInt                                                 as DataSatInt
-- import qualified Data.Set                                                    as DataSet
import qualified Data.Typeable                                                  as DataTypeable (Typeable)
import qualified Data.Text                                                      as DataText 
import qualified Data.Void                                                      as DataVoid (Void)
import qualified GHC.Natural                                                    as GHCNatural
import qualified Ledger                                                                            
import qualified Ledger.Tx                                                      as LedgerTx              
import qualified Ledger.Tx.CardanoAPI.Internal                                  as LedgerTxCardanoAPIInternal
-- import qualified Ledger.Ada                                                  as LedgerAda
-- import qualified Ledger.Scripts                                              as LedgerScripts
-- import qualified Ledger.Value                                                as LedgerValue
import qualified Ledger.Constraints                                             as LedgerConstraints
import qualified Ledger.Validation                                              as LedgerValidation
import qualified Ledger.TimeSlot                                                as LedgerTimeSlot
-- import qualified Ledger.Constraints.TxConstraints                            as LedgerConstraintsTxConstraints
-- import qualified Ledger.Constraints.ValidityInterval                         as LedgerValidityInterval
-- import qualified Ledger.Tx                                                   as LedgerTx 
import qualified Ledger.Tx.CardanoAPI                                           as LedgerTxCardanoAPI
import qualified Ledger.Tx.Constraints                                          as LedgerTxConstraints
-- import qualified Playground.Contract                                         as PlaygroundContract (mkSchemaDefinitions)
import qualified Plutus.Contract                                                as PlutusContract
-- import qualified Plutus.Script.Utils.Scripts                                 as UtilsScripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators                as UtilsTypedScriptsValidatorsV1 (RedeemerType, DatumType) -- TypedValidator, ValidatorTypes, mkTypedValidator, mkUntypedValidator, validatorAddress, validatorHash, validatorScript
-- import qualified Plutus.Script.Utils.V2.Scripts                               as UtilsScriptsV2
-- import qualified Plutus.V1.Ledger.Interval                                   as LedgerIntervalV1 (interval)
import qualified Plutus.V2.Ledger.Api                                           as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Address                                    as LedgerAddressV2
-- import qualified Plutus.V2.Ledger.Value                                      as LedgerValueV2
-- import qualified PlutusTx                                               
-- import qualified PlutusTx.Builtins.Class                                     as TxBuiltinsClass
import           PlutusTx.Prelude                                               hiding (unless)
-- import qualified PlutusTx.Ratio                                              as TxRatio
import qualified Prelude                                                        as P
import qualified Text.Printf                                                    as TextPrintf (printf)

----------------------------------------------------------------------------------------
-- Import Internos
----------------------------------------------------------------------------------------

import qualified MarketNFTV2.Helpers as Helpers
import qualified MarketNFTV2.HelpersOffChain  as HelpersOffChain
import qualified Utils

------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------
-- import qualified Control.Monad as ControlMonad (void)
-- import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
-- import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
-- import qualified GHC.Generics                        as GHCGenerics (Generic)

-- import qualified Plutus.Trace.Emulator                         as TraceEmulator
-- import qualified Data.ByteString.Lazy  as BSL
-- import qualified Data.Aeson  as Aeson
-- import qualified Control.Monad.IO.Class                                                     as MonadIOClass (MonadIO (..))

evalAndSubmitTx :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.MintingPolicy)] -> LedgerApiV2.Validator -> 
    LedgerConstraints.ScriptLookups DataVoid.Void -> LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void) -> 
    PlutusContract.Contract w s DataText.Text ()
evalAndSubmitTx listOfMintingScripts validator lookupsTx tx = do
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        PlutusContract.logWarn @P.String $ TextPrintf.printf "------------------  Submit Tx  -----------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------  mkTxConstraints     --------------------"
        !txUnBalanced <- PlutusContract.mkTxConstraints @DataVoid.Void lookupsTx tx
        !balanceTx <- PlutusContract.balanceTx txUnBalanced
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "txUnBalanced: %s" (P.show txUnBalanced)
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "balanceTx: %s" (P.show balanceTx)
        ------------------------
        let
            !(Ledger.CardanoApiTx someCardanoApiTx) = balanceTx
            -- emTx = _emulatorTx balanceTx -- el otro tipo de tx
            ------------------------
            getEmulatorEraTx :: Ledger.SomeCardanoApiTx -> CardanoApiShelley.Tx CardanoApiShelley.BabbageEra
            getEmulatorEraTx (LedgerTx.SomeTx tx' CardanoApiShelley.BabbageEraInCardanoMode) = tx'
            getEmulatorEraTx _                                     = error ()
            ------------------------
            !emulatorEraTx = getEmulatorEraTx someCardanoApiTx
            ------------------------
            !(CardanoApiShelley.ShelleyTx _ (AlonzoTx.ValidatedTx bodyTx AlonzoTxWitness.TxWitness {txrdmrs = AlonzoTxWitness.Redeemers redemersTx} (AlonzoTx.IsValid validTx) auxiliaryData)) = emulatorEraTx
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "BodyTx: %s" (P.show bodyTx)
            ------------------------
            !(CardanoApiShelley.ShelleyTx _ txVal) = emulatorEraTx
            ------------------------
            getSize :: ( DataTypeable.Typeable era, CardanoBinary.ToCBOR (CardanoLedgerCore.TxBody era), CardanoBinary.ToCBOR (CardanoLedgerCore.AuxiliaryData era) ) => AlonzoTx.ValidatedTx era -> P.Integer
            getSize !tx' = P.fromIntegral . LBS.length . CardanoBinary.serializeEncoding $ AlonzoTx.toCBORForSizeComputation tx'
            ------------------------
            !sizeTx = getSize txVal
            ------------------------
            !allRedeemers = DataMap.toList redemersTx
            ------------------------
            !(BabbageTxBody.TxBody 
                _spendInputs
                _collateralInputs
                _referenceInputs
                _outputs
                _collateralReturn
                _totalCollateral
                _certs
                _wdrls
                _txfee
                _vldt
                _update
                _reqSignerHashes
                _mint
                _scriptIntegrityHash
                _adHash
                _txnetworkid)   = bodyTx
            ----------------------
            getTxBodyContent :: LedgerTx.SomeCardanoApiTx -> CardanoApiShelley.TxBodyContent CardanoApiShelley.ViewTx CardanoApiShelley.BabbageEra
            getTxBodyContent (LedgerTx.CardanoApiEmulatorEraTx (CardanoApiShelley.Tx (CardanoApiShelley.TxBody bodyContent) _)) = bodyContent
            ----------------------
            !txBodyContent = getTxBodyContent someCardanoApiTx
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "TxBodyContent: %s" (P.show txBodyContent)
            ----------------------
            getTxBodyContentInputsReference :: CardanoApi.TxBodyContent ctx era -> [LedgerTx.TxIn]
            getTxBodyContentInputsReference CardanoApi.TxBodyContent {..} =
                let 
                    txInsReferenceToPlutusTxIns CardanoApi.TxInsReferenceNone = []
                    txInsReferenceToPlutusTxIns (CardanoApi.TxInsReference _ txIns'') =
                        fmap ((`LedgerTx.TxIn` Nothing) . LedgerTxCardanoAPIInternal.fromCardanoTxIn) txIns''
                in txInsReferenceToPlutusTxIns txInsReference
            getTxBodyContentSignatures :: CardanoApi.TxBodyContent ctx era -> [LedgerApiV2.PubKeyHash]
            getTxBodyContentSignatures CardanoApi.TxBodyContent {..} =
                case txExtraKeyWits of
                    CardanoApi.TxExtraKeyWitnessesNone ->
                        []
                    CardanoApi.TxExtraKeyWitnesses _ xs ->
                        LedgerTxCardanoAPIInternal.fromCardanoPaymentKeyHash <$> xs
            ----------------------
            !txIns' = LedgerTx.getCardanoTxInputs balanceTx -- CardanoApi.txIns
            !txInsReference' = getTxBodyContentInputsReference txBodyContent -- CardanoApi.txInsReference
            !txOuts' = LedgerTx.getCardanoTxOutRefs balanceTx -- CardanoApi.txOuts
            !txFee' = LedgerTx.getCardanoTxFee balanceTx -- LedgerTxCardanoAPIInternal.fromCardanoFee $ CardanoApi.txFee txBodyContent  -- LedgerTx.txFee emTx
            !txValidityRange' = LedgerTx.getCardanoTxValidityRange balanceTx -- CardanoApi.txValidityRange txBodyContent -- CardanoNodeEmulatorTimeSlot.slotRangeToPOSIXTimeRange slotConfig (LedgerTx.txValidRange emTx)
            !txWithdrawals' = LedgerApiV2.fromList [] -- (LedgerApiV2.StakingHash $ LedgerTx.withdrawalCredential w, LedgerTx.withdrawalAmount w) | w <- txWithdrawals'] -- LedgerShelleyTxBody.unWdrl _wdrls -- CardanoApi.txWithdrawals txBodyContent -- _wdrls -- CardanoApi.txWithdrawals txBodyContent -- LedgerTx.txWithdrawals emTx
            !txCertificates' = [] --_certs -- CardanoApi.txCertificates txBodyContent -- LedgerTx.certificateDcert <$> LedgerTx.txCertificates emTx
            !txMintValue' = LedgerTx.getCardanoTxMint balanceTx -- CardanoApi.txMintValue txBodyContent --LedgerTx.txMint emTx -- _mint
            --txInfoRedeemers' = [] -- LedgerTx.getCardanoTxRedeemers balanceTx -- LedgerTx.txRedeemers emTx -- [(red', LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData () )]
            !txInfoRedeemers'' = mapM (BabbageTxInfo.transRedeemerPtr bodyTx) allRedeemers
            -- filter only left from either redeemer
            !txInfoRedeemers' =  (\case {Left _ -> []; Right x -> x}) txInfoRedeemers''
            !txInfoData' = LedgerTx.getCardanoTxData balanceTx -- LedgerApiV2.fromList $ DataMap.toList $ LedgerTx.txData emTx
            !txInfoId' = LedgerTx.getCardanoTxId balanceTx -- LedgerTx.txId emTx
            !txInfoSignatories' = getTxBodyContentSignatures txBodyContent --BabbageTxBody.reqSignerHashes txBodyContent --_reqSignerHashes -- LedgerTx.txSignatures emTx
            -- CardanoApi.txInsCollateral = TxInsCollateralNone
            -- CardanoApi.txTotalCollateral
            -- CardanoApi.txReturnCollateral
            -- CardanoApi.txMetadata
            -- CardanoApi.txAuxScripts
            -- CardanoApi.txExtraKeyWits
            -- CardanoApi.txProtocolParams = BuildTxWith Nothing
            -- CardanoApi.txUpdateProposal
            -- CardanoApi.txScriptValidity
            ----------------------
            getDecoratedTxOut :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerTx.DecoratedTxOut
            getDecoratedTxOut txInput' = do
                decoratedTxOut' <- PlutusContract.txOutFromRef txInput'
                let decoratedTxOut = Helpers.fromJust decoratedTxOut'
                return decoratedTxOut
            ----------------------
            -- getValueFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.Value
            -- getValueFromTxOutRef txOutRef = do
            --     decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
            --     let decoratedTxOut = Helpers.fromJust decoratedTxOut'
            --     let value = Utils.getValueFromDecoratedTxOut decoratedTxOut 
            --     return value
            ----------------------
            getOutputDatumFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text LedgerApiV2.OutputDatum
            getOutputDatumFromTxOutRef txOutRef = do
                decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
                let decoratedTxOut = Helpers.fromJust decoratedTxOut'
                    ----------------------
                    toPlutusOutputDatum :: Maybe (LedgerApiV2.DatumHash, LedgerTx.DatumFromQuery) -> LedgerApiV2.OutputDatum
                    toPlutusOutputDatum Nothing                             = LedgerApiV2.NoOutputDatum
                    toPlutusOutputDatum (Just (_, LedgerTx.DatumInline d))  = LedgerApiV2.OutputDatum d
                    toPlutusOutputDatum (Just (_, LedgerTx.DatumInBody d))  = LedgerApiV2.OutputDatum d
                    toPlutusOutputDatum (Just (dh, _))                      = LedgerApiV2.OutputDatumHash dh
                    ----------------------
                let datum = toPlutusOutputDatum $ decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
                return datum
            --------------------
            getReferenceScriptFromTxOutRef :: LedgerApiV2.TxOutRef ->  PlutusContract.Contract w s DataText.Text (Maybe LedgerApiV2.ScriptHash)
            getReferenceScriptFromTxOutRef txOutRef = do
                decoratedTxOut' <- PlutusContract.txOutFromRef txOutRef
                let decoratedTxOut = Helpers.fromJust decoratedTxOut'
                let script' = decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutReferenceScript
                let script = Helpers.fromJust script'
                return $ P.maybe Nothing (Just . Ledger.scriptHash) script
            --------------------
            getTxInInfoFromLedgerTxInput :: Ledger.TxIn -> PlutusContract.Contract w s DataText.Text LedgerApiV2.TxInInfo
            getTxInInfoFromLedgerTxInput txInput = do
                let txOutRef = LedgerTx.txInRef txInput
                decoratedTxOut <- getDecoratedTxOut txOutRef
                let adddress = LedgerTx._decoratedTxOutAddress decoratedTxOut -- ControlLens.^? LedgerTx.decoratedTxOutAddress 
                let value = HelpersOffChain.getValueFromDecoratedTxOut decoratedTxOut 
                datum <- getOutputDatumFromTxOutRef txOutRef
                script <- getReferenceScriptFromTxOutRef txOutRef
                return $ LedgerApiV2.TxInInfo
                    (LedgerTx.txInRef txInput)
                    (LedgerApiV2.TxOut adddress value datum script)
            --------------------
            getTxOutFromLedgerTxOut :: (Ledger.TxOut, Ledger.TxOutRef) -> PlutusContract.Contract w s DataText.Text LedgerApiV2.TxOut
            getTxOutFromLedgerTxOut (txOut, _) = 
                --do
                -- decoratedTxOut <- getDecoratedTxOut txOutRef
                -- -- let adddress = Utils.cardanoAddressToAddress $ Ledger.txOutAddress txOut
                -- let adddress = LedgerTx._decoratedTxOutAddress decoratedTxOut 
                -- -- let value = LedgerTx.txOutValue txOut
                -- let value = Utils.getValueFromDecoratedTxOut decoratedTxOut 
                -- datum <- getOutputDatumFromTxOutRef txOutRef
                -- script <- getReferenceScriptFromTxOutRef txOutRef
                -- return $ LedgerApiV2.TxOut adddress value datum script
                --         -- txOutAddress :: Address	 
                --         -- txOutValue :: Value	 
                --         -- txOutDatum :: OutputDatum	 
                --         -- txOutReferenceScript :: Maybe ScriptHash	
                return $ LedgerTxCardanoAPIInternal.fromCardanoTxOutToPV2TxInfoTxOut $ LedgerTx.getTxOut  txOut 
            --------------------
            slotConfig :: LedgerTimeSlot.SlotConfig
            !slotConfig = LedgerTimeSlot.SlotConfig {
                scSlotLength = 1,
                scSlotZeroTime = 0
            }
        --------------------
        !mockTxInfoInputs <- traverse getTxInInfoFromLedgerTxInput txIns' -- :: [LedgerApiV2.TxInInfo]
        !mockTxReferenceInputs <- traverse getTxInInfoFromLedgerTxInput txInsReference' -- :: [LedgerApiV2.TxInInfo]
        !mockTxInfoOutputs <- traverse getTxOutFromLedgerTxOut txOuts' -- :: [LedgerApiV2.TxOut]
        ------------------
        let 
            formatTxInInfo [] = []
            formatTxInInfo txInsInfo = tail $ concat  [ ["------------------------", "TxInInfo " ++ P.show (n+1 :: Integer), P.show txInInfo] | (n, txInInfo) <- Helpers.enumerate txInsInfo]
        let 
            formatTxOut [] = []
            formatTxOut txOuts = tail $ concat  [ ["------------------------", "TxOut " ++ P.show (n+1 :: Integer), P.show txOut] |  (n, txOut) <- Helpers.enumerate txOuts]
        ---------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------- TxReferenceInputs -----------------------"
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "TxReferenceInputs"
        mapM_ (PlutusContract.logInfo @P.String) (formatTxInInfo mockTxReferenceInputs)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------- TxInfoInputs ------------"
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "TxInfoInputs"
        mapM_ (PlutusContract.logInfo @P.String) (formatTxInInfo mockTxInfoInputs)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------- TxInfoOutputs ------------------"
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "TxInfoOutputs"
        mapM_ (PlutusContract.logInfo @P.String) (formatTxOut mockTxInfoOutputs)
        ---------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------ Size ----------------------------------"
        if sizeTx > 16000 then 
            PlutusContract.throwError @DataText.Text $ Utils.stringToStrictText $ TextPrintf.printf "Tx is too big: %s" (P.show sizeTx) 
        else 
            PlutusContract.logInfo @P.String $ TextPrintf.printf "SizeTx: %s" (P.show sizeTx)
        ------------------------
        let
            !allUnits = map (\(_, (_, AlonzoScripts.ExUnits mem steps)) -> (mem, steps)) allRedeemers
            !sumUnitsMem = foldl (\acc (mem, _) -> acc `GHCNatural.plusNatural` mem) 0 allUnits
            !sumUnitsSteps = foldl (\acc (_, steps) -> acc `GHCNatural.plusNatural` steps) 0 allUnits
            formatUnits !list = concat  [ ["ExMemory " ++ P.show mem ++ ", ExCPU  " ++ P.show step] | (mem, step) <- list]
            ---------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------ Memory ----------------------------------"
        mapM_ (PlutusContract.logWarn @P.String) (formatUnits allUnits)
        PlutusContract.logWarn @P.String $ TextPrintf.printf "Total ExMemory %s, Total ExCPU  %s" (P.show sumUnitsMem) (P.show sumUnitsSteps)
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Valid Tx: %s" (P.show validTx)
        --PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        ------------------------   
        let
            mockTxInfoFee :: LedgerApiV2.Value
            mockTxInfoFee = txFee'
            ----------------------
            mockTxInfoMint :: LedgerApiV2.Value
            mockTxInfoMint = txMintValue'
            ----------------------
            mockTxInfoDCert :: [LedgerApiV2.DCert]
            mockTxInfoDCert = txCertificates'
            ----------------------
            mockTxInfoWdrl :: LedgerApiV2.Map LedgerApiV2.StakingCredential Integer
            mockTxInfoWdrl = txWithdrawals'
            ----------------------
            mockTxInfoValidRange :: LedgerApiV2.POSIXTimeRange
            mockTxInfoValidRange = LedgerTimeSlot.slotRangeToPOSIXTimeRange slotConfig txValidityRange' 
            ----------------------
            mockTxInfoSignatories :: [LedgerApiV2.PubKeyHash]
            mockTxInfoSignatories = txInfoSignatories'
            ----------------------
            mockTxInfoRedeemers :: LedgerApiV2.Map LedgerApiV2.ScriptPurpose LedgerApiV2.Redeemer
            mockTxInfoRedeemers = LedgerApiV2.fromList txInfoRedeemers'
            ----------------------
            mockTxInfoData :: LedgerApiV2.Map LedgerApiV2.DatumHash LedgerApiV2.Datum
            mockTxInfoData = LedgerApiV2.fromList $ DataMap.toList txInfoData' 
            ----------------------
            mockTxInfoId :: LedgerApiV2.TxId
            mockTxInfoId = txInfoId'
            ----------------------
            getTxInfo :: LedgerApiV2.TxInfo
            getTxInfo = LedgerApiV2.TxInfo 
                            mockTxInfoInputs
                            mockTxReferenceInputs
                            mockTxInfoOutputs
                            mockTxInfoFee
                            mockTxInfoMint
                            mockTxInfoDCert
                            mockTxInfoWdrl
                            mockTxInfoValidRange
                            mockTxInfoSignatories
                            mockTxInfoRedeemers
                            mockTxInfoData
                            mockTxInfoId
            ----------------------
            evalRedeemer :: (LedgerApiV2.ScriptPurpose, LedgerApiV2.Redeemer) -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.ExMemory, LedgerApiV2.ExCPU, P.Integer)
            evalRedeemer red = do
                let mockScriptPurpose = fst red -- LedgerApiV2.Minting $ LedgerApiV2.CurrencySymbol "fff"  --head $ DataMap.toList txInfoRedeemers' -- red' 
                case mockScriptPurpose of
                    LedgerApiV2.Minting mintingHash -> do
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------  evalRedeemer  Minting  ----------------------"
                        let
                            policy' = DataMap.lookup mintingHash $ DataMap.fromList listOfMintingScripts
                        case policy' of
                            Nothing -> do 
                                PlutusContract.logInfo @P.String $ "Redeemer: mintingHash not found"
                                return (0, 0, 0)
                            Just policy -> do
                                let    
                                    mockCtx :: LedgerApiV2.ScriptContext
                                    mockCtx = LedgerApiV2.ScriptContext getTxInfo mockScriptPurpose
                                    ----------------------
                                    !eval_data = [ LedgerApiV2.toData $ snd red , LedgerApiV2.toData mockCtx]
                                    (eval_log, eval_err, eval_size) = Utils.evaluateScriptMint policy eval_data
                                --------------------------------
                                -- PlutusContract.logInfo @P.String $ "allRedeemers: " ++  P.show allRedeemers
                                PlutusContract.logInfo @P.String $ "ScriptPurpose: " ++  P.show mockScriptPurpose
                                PlutusContract.logInfo @P.String $ "Redeemer: " ++  P.show red
                                PlutusContract.logInfo @P.String $ "Log: " ++  P.show eval_log
                                case eval_err of
                                    Left _ -> do
                                        PlutusContract.logInfo @P.String $ "Eval Error , Size: " ++  P.show eval_size
                                        return (0, 0, eval_size)
                                    Right exbudget -> do
                                        PlutusContract.logInfo @P.String $ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++ "Size: " ++ P.show eval_size
                                        return (LedgerApiV2.exBudgetMemory exbudget,LedgerApiV2.exBudgetCPU exbudget, eval_size)
                                -- PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
                    LedgerApiV2.Spending _ -> do
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------  evalRedeemer  Spending ----------------------------"
                        let
                            mockCtx :: LedgerApiV2.ScriptContext
                            mockCtx = LedgerApiV2.ScriptContext getTxInfo mockScriptPurpose
                            ----------------------
                            -- TODO: falta el datum del validador principal
                            !eval_data = [ LedgerApiV2.toData (), LedgerApiV2.toData $ snd red , LedgerApiV2.toData mockCtx]
                            (eval_log, eval_err, eval_size) = Utils.evaluateScriptValidator validator eval_data
                            --------------------------------
                        -- PlutusContract.logInfo @P.String $ "allRedeemers: " ++  P.show allRedeemers
                        PlutusContract.logInfo @P.String $ "ScriptPurpose: " ++  P.show mockScriptPurpose
                        PlutusContract.logInfo @P.String $ "Redeemer: " ++  P.show red
                        PlutusContract.logInfo @P.String $ "Log: " ++  P.show eval_log
                        case eval_err of
                            Left _ -> do
                                PlutusContract.logInfo @P.String $ "Eval Error , Size: " ++  P.show eval_size
                                return (0, 0, eval_size)
                            Right exbudget -> do
                                PlutusContract.logInfo @P.String $ P.show (LedgerApiV2.exBudgetMemory exbudget) ++ ", " ++ P.show (LedgerApiV2.exBudgetCPU exbudget) ++ ", " ++ "Size: " ++ P.show eval_size
                                return (LedgerApiV2.exBudgetMemory exbudget, LedgerApiV2.exBudgetCPU exbudget, eval_size)
                    LedgerApiV2.Rewarding _ -> 
                        return (0, 0, 0)
                    LedgerApiV2.Certifying _  -> 
                        return (0, 0, 0)
                        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        --------------------
        resultEvalRedemeers <- mapM evalRedeemer txInfoRedeemers'
        let !sumsEvals = foldl (\(mem', steps', size') ( mem, steps, size) -> (mem' P.+ mem, steps' P.+ steps, size' + size)) (0, 0, 0)  resultEvalRedemeers
        PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------  evalRedeemer Totals ----------------------------"
        let 
            getFst (fst', _, _) = fst'
            getSnd (_, snd', _) = snd'
            getThd (_, _, thd') = thd'
        PlutusContract.logInfo @P.String $ TextPrintf.printf "Total ExMemory %s, Total ExCPU  %s, Total Size  %s" (P.show (getFst sumsEvals)) (P.show (getSnd sumsEvals)) (P.show (getThd sumsEvals))
        --------------------
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------  submitBalancedTx     --------------------"
        !submittedTx <- PlutusContract.submitBalancedTx balanceTx
        PlutusContract.logInfo @P.String $ TextPrintf.printf "SubmitedTx (txId: %s)" (P.show $ Ledger.getCardanoTxId submittedTx)
        !cSlot <- PlutusContract.currentNodeClientSlot
        !params <- PlutusContract.getParams 
        let  (Right cUtxoIndex) =  LedgerValidation.fromPlutusIndex $ Ledger.UtxoIndex $ LedgerTxConstraints.unBalancedTxUtxoIndex txUnBalanced -- $ Ledger.UtxoIndex DataMap.empty --mempty  -- Ledger.UtxoIndex DataMap.empty --LedgerTxCardanoAPI.fromPlutusIndex mempty -- (Ledger.UtxoIndex utxo)
        -- PlutusContract.logInfo @P.String $ TextPrintf.printf "params: %s" (P.show params)
        let validateCardanoTx = LedgerValidation.validateCardanoTx params cSlot cUtxoIndex submittedTx
        case validateCardanoTx of
            -- Left (validationPhase, validationError) -> 
            Left err -> 
                -- PlutusContract.throwError @DataText.Text $ Utils.stringToStrictText $ TextPrintf.printf "validateCardanoTx (txId: %s) - ERROR: %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show err)
                --PlutusContract.logError @P.String $ P.show err
                PlutusContract.throwError @DataText.Text $ Utils.stringToStrictText $ TextPrintf.printf "ERROR: %s" (P.show err)
            Right mp -> do
                PlutusContract.logWarn @P.String $ TextPrintf.printf "validateCardanoTx (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show mp)
                !txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
                PlutusContract.logWarn @P.String $ TextPrintf.printf "ConfirmedTx (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)
                PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"   
        PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------------"   

--------------------------------------------------------------------------------------------