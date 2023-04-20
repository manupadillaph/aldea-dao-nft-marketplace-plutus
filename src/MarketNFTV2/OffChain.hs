-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- -- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE NoImplicitPrelude          #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE AllowAmbiguousTypes        #-}


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
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
-- {-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores         #-}

--------------------------------------------------------------------------------
module MarketNFTV2.OffChain where
--------------------------------------------------------------------------------
-- import qualified Control.Lens                        as ControlLens
import qualified Control.Monad                       as Monad 
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Map                            as DataMap
-- import qualified Data.Maybe                           as DataMaybe
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (getCardanoTxId, pubKeyHashAddress, unPaymentPubKeyHash, PaymentPubKeyHash(..) ) 
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Address                      as LedgerAddress
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Tx                           as LedgerTx
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.PAB.Effects.Contract.Builtin                    as PABEffectsContractBuiltin 

import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (interval) 
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
-- import qualified PlutusTx.AssocMap                                 as TxAssocMap
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Prettyprinter                                 
import qualified Schema                              
import qualified Text.Printf                         as TextPrintf (printf)
--------------------------------------------------------------------------------
import qualified Utils
import qualified MarketNFTV2.Types        as T 
import qualified MarketNFTV2.Helpers      as Helpers
import qualified MarketNFTV2.OnChain      as OnChain
import qualified MarketNFTV2.HelpersOffChain      as HelpersOffChain  
import qualified MarketNFTV2.OffChainEval      as OffChainEval  
--------------------------------------------------------------------------------

instance Schema.ToSchema LedgerApiV2.Validator where
  toSchema = Schema.FormSchemaUnit

-- TODO: para cuando vuelva a usar plutus-1.1.0, tengo que desactivar esto
instance Schema.ToSchema  LedgerAddress.Address where
  toSchema = Schema.FormSchemaUnit

instance Schema.ToSchema   LedgerApiV2.MintingPolicy where
  toSchema = Schema.FormSchemaUnit

--------------------------------------------------------------------------------

data PABParams = PABParams
    {
        ppProtocolID_TxOutRef :: LedgerApiV2.TxOutRef,
        ppValidatorProtocol :: LedgerApiV2.Validator,
        ppValidatorProtocolAddress :: LedgerAddress.Address,
        ppValidatorProtocolHash :: LedgerApiV2.ValidatorHash,
        ppValidator :: LedgerApiV2.Validator,
        ppValidatorAddress :: LedgerAddress.Address,
        ppValidatorHash :: LedgerApiV2.ValidatorHash,
        ppProtocolID_Policy :: LedgerApiV2.MintingPolicy,
        ppProtocolID_CS :: LedgerApiV2.CurrencySymbol

    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data PABParamsExtras = PABParamsExtras {
    ePABParams :: PABParams,
    eValidatorProtocol :: P.String,
    eValidatorProtocolAddressTestnet :: P.String,
    eValidatorProtocolAddressMainnet :: P.String,
    eValidator :: P.String,
    eValidatorAddressTestnet :: P.String,
    eValidatorAddressMainnet :: P.String,
    ePolicy_ProtocolID :: P.String
} deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

------------------------------------------------------------------------------------------

newtype PABBalanceAtScriptParams = PABBalanceAtScriptParams
    { 
        pbParams :: PABParams
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

newtype PABSplitUtxOParams = PABSplitUtxOParams
    { 
        psuSplitAmount :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABMintFreeParams = PABMintFreeParams
    { 
        -- pmmfPABParams :: PABParams, 
        pmmfMintPolicyNum :: Integer,
        pmmfMintTokenNameBase :: BuiltinByteString,
        pmmfMintDiifTokenNameCount :: Integer,
        pmmfMintAmount :: Integer
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema, Schema.ToSchema)

------------------------------------------------------------------------------------------

data PABPrepareProtocolParams = PABPrepareProtocolParams
    {
        ppParams :: PABParams,
        ppAldeaPkh :: LedgerApiV2.PubKeyHash,
        ppAldeaPct :: Integer,
        ppProtocolPkh :: LedgerApiV2.PubKeyHash,
        ppProtocolPct :: Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data PABUpdateProtocolParams = PABUpdateProtocolParams
    {
        upParams :: PABParams,
        upAldeaPkh :: LedgerApiV2.PubKeyHash,
        upAldeaPct :: Integer,
        upProtocolPkh :: LedgerApiV2.PubKeyHash,
        upProtocolPct :: Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data PABSellNFTParams = PABSellNFTParams
    {
        spParams :: PABParams,
        spNFT   :: T.NFT,
        spPriceADA  :: Maybe Integer,
        spPriceALDEA  :: Maybe Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data PABBuyNFTParams = PABBuyNFTParams
    {
        bpParams :: PABParams,
        bpNFTsBuy :: [T.NFTBuy]
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data PABGetBackNFTParams = PABGetBackNFTParams
    {
        gbpParams :: PABParams,
        gbpNFT   :: T.NFT
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

--------------------------------------------------------------------------------


examplePABParams :: PABParams
examplePABParams = PABParams
    { 
    }

exampleBalanceAtScriptParams :: PABBalanceAtScriptParams
exampleBalanceAtScriptParams = PABBalanceAtScriptParams
    { 
    }

exampleSplitUtxOParams :: PABSplitUtxOParams
exampleSplitUtxOParams = PABSplitUtxOParams
    { 
    }

exampleMintFreeParams :: PABMintFreeParams
exampleMintFreeParams = PABMintFreeParams
    { 
    }

examplePrepareProtocolParams :: PABPrepareProtocolParams
examplePrepareProtocolParams = PABPrepareProtocolParams
    { 
    }

exampleUpdateProtocolParams :: PABUpdateProtocolParams
exampleUpdateProtocolParams = PABUpdateProtocolParams
    { 
    }

exampleSellNFTParams :: PABSellNFTParams
exampleSellNFTParams = PABSellNFTParams
    { 
    }

exampleBuyNFTParams :: PABBuyNFTParams
exampleBuyNFTParams = PABBuyNFTParams
    { 
    }

exampleGetBackNFTParams :: PABGetBackNFTParams
exampleGetBackNFTParams = PABGetBackNFTParams
    { 
    }

------------------------------------------------------------------------------------------

balanceAtScript :: PABBalanceAtScriptParams -> PlutusContract.Contract w s DataText.Text ()
balanceAtScript PABBalanceAtScriptParams {..} = PlutusContract.handleError HelpersOffChain.handleContractError $ do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logWarn @P.String $ TextPrintf.printf "--------------------------- Balance At Script : Init ------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange 
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    let
        -- !validatorAddress = ppValidatorAddress pbParams
        !addressValidatorProtocol = ppValidatorProtocolAddress pbParams
    ----------------------
        !protocolID_CS = ppProtocolID_CS pbParams
        !protocolID_AC = LedgerValue.AssetClass (protocolID_CS, T.protocolID_TN)
    ----------------------
    -- !uTxOsAtValidator <- PlutusContract.utxosAt validatorAddress
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----------------"
    ---------------------
    uTxOAtScript_With_ProtocolDatum' <- HelpersOffChain.findUtxoInValidatorWithNFT addressValidatorProtocol protocolID_AC 
    ---------------------
    case uTxOAtScript_With_ProtocolDatum' of
        Nothing -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Can't find any uTxO with Protocol Datum"
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
        Just uTxOAtScript_With_ProtocolDatum -> do
            let
                Just protocolDatum = HelpersOffChain.getDatumFromDecoratedTxOut @T.DatumProtocol $ snd uTxOAtScript_With_ProtocolDatum
                !valueProtocolDatum = HelpersOffChain.getValueFromDecoratedTxOut $ snd uTxOAtScript_With_ProtocolDatum
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Protocol Datum UTxO: %s" (P.show $ fst uTxOAtScript_With_ProtocolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Protocol Datum: %s" (P.show protocolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Protocol Datum Value: %s" (P.show valueProtocolDatum)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "----"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"

------------------------------------------------------------------------------------------

splitUtxO :: PABSplitUtxOParams -> PlutusContract.Contract w s DataText.Text ()
splitUtxO PABSplitUtxOParams {..} = PlutusContract.handleError HelpersOffChain.handleContractError $ do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logWarn @P.String $ TextPrintf.printf "------------------------------------- Split UTxO : Init ------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    uTxOsAtUser <- PlutusContract.utxosAt userAdds
    ---------------------
    let
        !splitAmount  = psuSplitAmount
    ---------------------
        !value_For_SplitAmount  = LedgerAda.lovelaceValueOf splitAmount
    ---------------------
        lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtUser 
        tx =
            LedgerConstraints.mustPayToPubKey userPPKH value_For_SplitAmount P.<> 
            LedgerConstraints.mustPayToPubKey userPPKH value_For_SplitAmount P.<> 
            LedgerConstraints.mustBeSignedBy userPPKH
    ------------------------
    submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Split UTxO (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

------------------------------------------------------------------------------------------

mintFree :: PABMintFreeParams -> PlutusContract.Contract w s DataText.Text ()
mintFree PABMintFreeParams{..} = PlutusContract.handleError HelpersOffChain.handleContractError $ do
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logWarn @P.String $ TextPrintf.printf "--------------------------- Mint : Init ---------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    ---------------------
    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        -- !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    !userAdds <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAdds
    ---------------------
    let
        !mintPolicyNum  = pmmfMintPolicyNum
        !mintTokenNameBase  = pmmfMintTokenNameBase
        !mintDifTokenNameCount  = pmmfMintDiifTokenNameCount
        !mintAmount  = pmmfMintAmount
    ---------------------
        !policy_MintFree = OnChain.policy_Free mintPolicyNum
    ---------------------
        !mintFree_CS = Utils.getCurSymbolOfPolicy policy_MintFree
    ---------------------
        validTimeRange :: LedgerApiV2.POSIXTime
        !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
        !intervalOffset1 = 1000
        !intervalOffset2 = validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    ---------------------
        !mintFree_TNS =
            if mintDifTokenNameCount > 1 then do
                [ LedgerApiV2.TokenName (mintTokenNameBase <> Helpers.intToBBS num)  | num <- [1..mintDifTokenNameCount] :: [Integer]]
            else
                [ LedgerApiV2.TokenName mintTokenNameBase ]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "mintFree TNS: %s" (P.show mintFree_TNS)
    ---------------------
    let
        !value_For_Mint_MintFree = foldl (<>) (LedgerAda.lovelaceValueOf 0) ([
            let
                !mintFree_AC = LedgerValue.AssetClass (mintFree_CS, mintFree_TN')
            in
                LedgerValue.assetClassValue mintFree_AC mintAmount | mintFree_TN' <- mintFree_TNS
            ])

        !lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtUser P.<>
            LedgerConstraints.plutusV2MintingPolicy policy_MintFree
        !tx =
            LedgerConstraints.mustMintValue value_For_Mint_MintFree P.<>
            LedgerConstraints.mustValidateIn validityRange      P.<>    
            LedgerConstraints.mustBeSignedBy userPPKH
    ------------------------
    !submittedTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    !txStatus <- PlutusContract.awaitTxStatusChange $ Ledger.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "txStatus Mint Free (txId: %s): %s" (P.show $ Ledger.getCardanoTxId submittedTx) (P.show txStatus)

------------------------------------------------------------------------------------------

prepareProtocol :: PABPrepareProtocolParams -> PlutusContract.Contract w s DataText.Text ()
prepareProtocol PABPrepareProtocolParams{..} = do

    let hashValidatorProtocol = ppValidatorProtocolHash ppParams
    let policy_ProtocolID = ppProtocolID_Policy ppParams
    let protocolID_CS = ppProtocolID_CS ppParams
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "-----------------------------------"

    (now,_) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "-----------------------------------"

    creatorPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    -- let creatorPkh = Ledger.unPaymentPubKeyHash creatorPPKH
    let creatorAdds = Ledger.pubKeyHashAddress creatorPPKH Nothing
    creatorUTxOs <- PlutusContract.utxosAt creatorAdds
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "-----------------------------------"

    let 
        !protocolID_AC = LedgerValue.AssetClass (protocolID_CS, T.protocolID_TN)

        valueNFT = LedgerValue.assetClassValue protocolID_AC 1
        minADA = HelpersOffChain.calculateMinAdaOfValue valueNFT True
        valueNFTPlusADA = valueNFT <> LedgerAda.lovelaceValueOf minADA

        datum = T.DatumProtocol
            {
                aldeaPkh = ppAldeaPkh,
                aldeaStakeCredential = Nothing,
                aldeaPct = ppAldeaPct,
                protocolPkh = ppProtocolPkh,
                protocolStakeCredential = Nothing,
                protocolPct = ppProtocolPct
            }

        validTimeRange :: LedgerApiV2.POSIXTime
        !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
        !intervalOffset1 = 1000
        !intervalOffset2 = validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "-----------------------------------2"
        
    let   
        (lookupsTx_Mint_ProtocolID, tx_Mint_ProtocolID) = HelpersOffChain.mintNFT_With_TxOut creatorUTxOs policy_ProtocolID (ppProtocolID_TxOutRef ppParams) Nothing valueNFT validityRange creatorPPKH

        lookupsTx = 
            LedgerConstraints.unspentOutputs creatorUTxOs  P.<> 
            lookupsTx_Mint_ProtocolID 

        tx = 
            tx_Mint_ProtocolID  P.<> 
            LedgerConstraints.mustPayToOtherScriptWithInlineDatum hashValidatorProtocol (LedgerApiV2.Datum $ PlutusTx.toBuiltinData datum) valueNFTPlusADA  P.<>
            LedgerConstraints.mustValidateIn validityRange          P.<>
            LedgerConstraints.mustBeSignedBy creatorPPKH                

    ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "-----------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "Prepare Protocol - Submited - Datum: %s - Value: %s" (P.show datum) (P.show valueNFTPlusADA)
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "-----------------------------------"

--------------------------------------------------------------------------------

updateProtocol :: PABUpdateProtocolParams -> PlutusContract.Contract w s DataText.Text ()
updateProtocol PABUpdateProtocolParams{..} = do

    let hashValidatorProtocol = ppValidatorProtocolHash upParams
    let codeValidatorProtocol = ppValidatorProtocol upParams
    let addressValidatorProtocol = ppValidatorProtocolAddress upParams
    let protocolID_CS = ppProtocolID_CS upParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    creatorPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    -- let creatorPkh = Ledger.unPaymentPubKeyHash creatorPPKH
    let creatorAdds = Ledger.pubKeyHashAddress creatorPPKH Nothing
    creatorUTxOs <- PlutusContract.utxosAt creatorAdds 

    let  !protocolID_AC = LedgerValue.AssetClass (protocolID_CS, T.protocolID_TN)
    utxo <- HelpersOffChain.findUtxoInValidatorWithNFT addressValidatorProtocol protocolID_AC 
    case utxo of
        Nothing -> 
            PlutusContract.throwError "Cannot find Utxo with ProtocolID"
        Just (oref, decoratedTxOut) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Update Protocol - Redeem Utxo: %s ---------------------------" (P.show oref)
            let
                -- Just dOld = getDatumFromDecoratedTxOut @DatumProtocol decoratedTxOut

                datum = T.DatumProtocol { 
                    aldeaPkh = upAldeaPkh,
                    aldeaStakeCredential = Nothing,
                    aldeaPct = upAldeaPct,
                    protocolPkh = upProtocolPkh,
                    protocolStakeCredential = Nothing,
                    protocolPct = upProtocolPct
                }

                valueProtocol = HelpersOffChain.getValueFromDecoratedTxOut decoratedTxOut

                redeemer = T.RedeemerUpdateDatum 
                redeemerLedger  = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

                validTimeRange :: LedgerApiV2.POSIXTime
                !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
                !intervalOffset1 = 1000
                !intervalOffset2 = validTimeRange - 1000
                !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)

                lookupsTx =
                    LedgerConstraints.plutusV2OtherScript codeValidatorProtocol P.<>
                    LedgerConstraints.unspentOutputs (DataMap.singleton oref decoratedTxOut) P.<>
                    LedgerConstraints.unspentOutputs creatorUTxOs

                tx =
                    LedgerConstraints.mustSpendScriptOutput oref redeemerLedger P.<>
                    LedgerConstraints.mustPayToOtherScriptWithInlineDatum hashValidatorProtocol (LedgerApiV2.Datum $ PlutusTx.toBuiltinData datum) valueProtocol P.<>
                    LedgerConstraints.mustValidateIn validityRange     P.<>
                    LedgerConstraints.mustBeSignedBy creatorPPKH           

            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf  "--------------------------- Update Protocol - Submited - Datum: %s - Value: %s ---------------------------" (P.show datum) (P.show valueProtocol)

--------------------------------------------------------------------------------

sell :: PABSellNFTParams -> PlutusContract.Contract w s DataText.Text ()
sell PABSellNFTParams{..} = do

    let hashValidator = ppValidatorHash spParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    sellerPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let sellerPkh = Ledger.unPaymentPubKeyHash sellerPPKH
    let sellerAdds = Ledger.pubKeyHashAddress sellerPPKH Nothing
    sellerUTxOs <- PlutusContract.utxosAt sellerAdds

    let 
        (cs, tn) = LedgerValue.unAssetClass spNFT

        valueNFT = LedgerValue.singleton cs tn 1 
        minADA = HelpersOffChain.calculateMinAdaOfValue valueNFT True
        valueNFTPlusADA = valueNFT <> LedgerAda.lovelaceValueOf minADA

        datum = T.DatumMarket
            {
                dSeller = sellerPkh,
                dSellerStakeCredential = Nothing,
                dNFT = spNFT,
                dPriceADA = spPriceADA,
                dPriceALDEA = spPriceALDEA,
                dMinADA = minADA
            }

        validTimeRange :: LedgerApiV2.POSIXTime
        !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
        !intervalOffset1 = 1000
        !intervalOffset2 = validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
        
        lookupsTx = 
            LedgerConstraints.unspentOutputs sellerUTxOs 

        tx = 
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx hashValidator (LedgerApiV2.Datum $ PlutusTx.toBuiltinData datum) valueNFTPlusADA  P.<>
            LedgerConstraints.mustValidateIn validityRange    P.<>
            LedgerConstraints.mustBeSignedBy sellerPPKH               

    ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf  "--------------------------- Sell Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show datum) (P.show valueNFT)

--------------------------------------------------------------------------------

buy :: forall w s. PABBuyNFTParams ->  PlutusContract.Contract w s DataText.Text ()
buy PABBuyNFTParams{..} = do

    let codeValidator = ppValidator bpParams
    let addressValidator = ppValidatorAddress bpParams
    let addressValidatorProtocol = ppValidatorProtocolAddress bpParams
    let protocolID_CS = ppProtocolID_CS bpParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    buyerPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let buyerPkh = Ledger.unPaymentPubKeyHash buyerPPKH
    let buyerAdds = Ledger.pubKeyHashAddress buyerPPKH Nothing
    buyerUTxOs <- PlutusContract.utxosAt buyerAdds 

    let  !protocolID_AC = LedgerValue.AssetClass (protocolID_CS, T.protocolID_TN)
    utxoProtocol' <- HelpersOffChain.findUtxoInValidatorWithNFT addressValidatorProtocol protocolID_AC 
    case utxoProtocol' of
        Nothing -> 
            PlutusContract.throwError "Cannot find Utxo with ProtocolID"
        Just utxoProtocol -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Found Utxo with ProtocolID: %s" (P.show utxoProtocol)

            let 
                !aldea_AC = LedgerValue.AssetClass (T.aldea_CS, T.aldea_TN)
                
                Just dProtocol = HelpersOffChain.getDatumFromDecoratedTxOut @T.DatumProtocol (snd utxoProtocol)

                !aldeaPkh = T.aldeaPkh dProtocol
                !aldeaPct = T.aldeaPct dProtocol
                !protocolPkh = T.protocolPkh dProtocol
                !protocolPct = T.protocolPct dProtocol

            PlutusContract.logInfo @P.String $ TextPrintf.printf "aldeaPkh: %s" (P.show aldeaPkh)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "aldeaPct: %s" (P.show aldeaPct)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "protocolPkh: %s" (P.show protocolPkh)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "protocolPct: %s" (P.show protocolPct)
            
            let
                getData :: T.NFTBuy -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, T.DatumMarket, LedgerApiV2.Value, LedgerApiV2.Value, Integer, Integer)
                getData nftBuy = do

                    PlutusContract.logInfo @P.String $ TextPrintf.printf "getData: %s" (P.show nftBuy)

                    let 
                        nft = T.nNFT nftBuy
                        swPayADA = T.nSwPayADA nftBuy

                    utxo <- HelpersOffChain.findUtxoInValidatorWithNFT addressValidator nft 
                    
                    case utxo of
                        Nothing -> 
                            PlutusContract.throwError $ Utils.stringToStrictText $ "Cannot find NFT in Validator: " ++ P.show nft
                        Just (oref, decoratedTxOut) -> do
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Found NFT in Validator: %s" (P.show (oref, decoratedTxOut))
                            let
                                Just dOld = HelpersOffChain.getDatumFromDecoratedTxOut @T.DatumMarket decoratedTxOut

                                !priceADA' = T.dPriceADA dOld
                                !priceALDEA' = T.dPriceALDEA dOld
                                !minADA = T.dMinADA dOld

                                (cs, tn) = LedgerValue.unAssetClass nft
                                vForBuyer     = LedgerValue.singleton cs tn 1 

                                -- minAda = HelperscalculateMinAdaOfValue vGetNFT True
                                -- vGetNFTPlusAda = vGetNFT <> LedgerAda.lovelaceValueOf minAda
                                
                            (vForSellerPlusADA, comissionsAldea'', comissionsProtocol'') <-
                                if swPayADA == 0 then
                                    -- si el comprador quiere pagar con ALDEA
                                    case priceALDEA' of
                                        Nothing -> 
                                            PlutusContract.throwError "Cant buy NFT with ALDEA Tokens"
                                        Just priceALDEA -> do
                                            return (LedgerValue.assetClassValue aldea_AC priceALDEA <>  LedgerAda.lovelaceValueOf minADA, T.minComission, T.minComission)
                                else
                                    -- si el comprador quiere pagar con ADA
                                    case priceADA' of
                                        Nothing -> 
                                            PlutusContract.throwError "Cant buy NFT with ADA"
                                        Just priceADA -> do
                                            let 
                                                !comissionsAldea' = aldeaPct * priceADA `divide` (100 * 100) -- usando puntos basicos para no perder precision
                                                !comissionsAldea = if comissionsAldea' < T.minComission then T.minComission else comissionsAldea' -- minimo 1 ADA de comisiones para Aldea
                                                !comissionsProtocol' = protocolPct * priceADA `divide` (100 * 100) -- usando puntos basicos para no perder precision
                                                !comissionsProtocol = if comissionsProtocol' < T.minComission then T.minComission else comissionsProtocol' -- minimo 1 ADA de comisiones para Protocol
                                                !price_For_Seller = priceADA -- - (comissionsAldea + comissionsProtocol) 
                                                !value_For_Seller = LedgerAda.lovelaceValueOf price_For_Seller <> LedgerAda.lovelaceValueOf minADA
                                            return (value_For_Seller, comissionsAldea, comissionsProtocol)
                            
                            return (oref, decoratedTxOut, dOld, vForBuyer, vForSellerPlusADA, comissionsAldea'', comissionsProtocol'')  

                getDatas :: [T.NFTBuy] -> PlutusContract.Contract w s DataText.Text [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, T.DatumMarket, LedgerApiV2.Value, LedgerApiV2.Value, Integer, Integer)]
                getDatas nftBuys = do
                    Monad.forM nftBuys getData

            !buyInfo <- getDatas bpNFTsBuy 

            PlutusContract.logInfo @P.String $ TextPrintf.printf "buyInfo: %s" (P.show buyInfo)
            let 
                !comissions = foldl (\(a1, a2) (b1, b2) -> (a1 + b1, a2 + b2)) (0, 0) [ (a, b) | (_ ,_, _, _, _, a, b ) <- buyInfo]
                !forAldea = LedgerAda.lovelaceValueOf $ fst comissions
                !forProtocol = LedgerAda.lovelaceValueOf $ snd comissions


            PlutusContract.logInfo @P.String $ TextPrintf.printf "forProtocol = %s" (P.show forProtocol)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "forAldea = %s" (P.show forAldea)

            let   

                !vForBuyer = foldl (<>) (LedgerAda.lovelaceValueOf 0) [ v | (_, _ ,_ ,v ,_ , _, _) <- buyInfo]
                
                !minADA = HelpersOffChain.calculateMinAdaOfValue vForBuyer True
                !vForBuyerPlusAda = vForBuyer <> LedgerAda.lovelaceValueOf minADA
                
                !redeemer = T.RedeemerBuyerBuyNFT T.RedeemerBuyerBuyNFTTypo {
                    T.rbbNFTsBuy = bpNFTsBuy
                } 
                
                !redeemerLedger  = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

                validTimeRange :: LedgerApiV2.POSIXTime
                !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
                !intervalOffset1 = 1000
                !intervalOffset2 = validTimeRange - 1000
                !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)

                lookupsTx =
                    LedgerConstraints.plutusV2OtherScript codeValidator P.<>
                    LedgerConstraints.unspentOutputs (DataMap.fromList [ (oref, d) | (oref,d,_,_,_,_,_) <- buyInfo] ) P.<>
                    LedgerConstraints.unspentOutputs (DataMap.fromList [utxoProtocol]) P.<>
                    LedgerConstraints.unspentOutputs buyerUTxOs

                tx =
                    LedgerConstraints.mustReferenceOutput (fst utxoProtocol) P.<>
                    -- LedgerConstraints.mustSpendScriptOutput oref redeemerLedger P.<>
                    mconcat [LedgerConstraints.mustSpendScriptOutput oref redeemerLedger | (oref,_,_,_,_,_,_) <- buyInfo ] P.<>
                    -- LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash $ T.dSeller dOld) vGetADA  P.<>
                    mconcat [LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash $ T.dSeller dat) vForSellerPlusADA | (_,_,dat,_,vForSellerPlusADA,_,_) <- buyInfo ] P.<>
                    LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash buyerPkh) vForBuyerPlusAda  P.<>
                    LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash aldeaPkh) forAldea  P.<>
                    LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash protocolPkh) forProtocol  P.<>
                    LedgerConstraints.mustValidateIn validityRange P.<>
                    LedgerConstraints.mustBeSignedBy buyerPPKH               

            OffChainEval.evalAndSubmitTx [] codeValidator lookupsTx tx

            -- --PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Buy Endpoint ---------------------------" 
            -- ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
            -- Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            -- PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Buy EndPoint - Submited -------------------------"

--------------------------------------------------------------------------------

getback :: forall w s. PABGetBackNFTParams ->  PlutusContract.Contract w s DataText.Text ()
getback PABGetBackNFTParams{..} = do

    let codeValidator = ppValidator gbpParams
    let addressValidator = ppValidatorAddress gbpParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    sellerPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let sellerPkh = Ledger.unPaymentPubKeyHash sellerPPKH
    let sellerAdds = Ledger.pubKeyHashAddress sellerPPKH Nothing
    utxosAtseller <- PlutusContract.utxosAt sellerAdds

    utxo <- HelpersOffChain.findUtxoInValidatorWithNFT addressValidator gbpNFT
    case utxo of
        Nothing -> PlutusContract.throwError "Cannot find NFT in Validator"
        Just (oref, decoratedTxOut) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)

            let
                --Just dOld = getDatumFromDecoratedTxOut @DatumMarket decoratedTxOut

                (cs, tn) = LedgerValue.unAssetClass gbpNFT

                vGetNFT     = LedgerValue.singleton cs tn 1 
                minAda = HelpersOffChain.calculateMinAdaOfValue vGetNFT True
                vGetNFTPlusAda = vGetNFT <> LedgerAda.lovelaceValueOf minAda

                redeemer = T.RedeemerSellerGetBackNFT
                redeemerLedger  = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

                validTimeRange :: LedgerApiV2.POSIXTime
                !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
                !intervalOffset1 = 1000
                !intervalOffset2 = validTimeRange - 1000
                !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
                
                lookupsTx =
                    LedgerConstraints.plutusV2OtherScript codeValidator P.<>
                    LedgerConstraints.unspentOutputs (DataMap.singleton oref decoratedTxOut) P.<>
                    LedgerConstraints.unspentOutputs utxosAtseller

                tx =
                    LedgerConstraints.mustSpendScriptOutput oref redeemerLedger P.<>
                    LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash sellerPkh) vGetNFTPlusAda P.<>
                    LedgerConstraints.mustValidateIn validityRange  P.<>
                    LedgerConstraints.mustBeSignedBy sellerPPKH    

            --PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack EndPoint ---------------------------" 
            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookupsTx tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack EndPoint - Submited -------------------------"

--------------------------------------------------------------------------------

type ValidatorSchema =
        PlutusContract.Endpoint "balanceAtScript" PABBalanceAtScriptParams PlutusContract..\/
        PlutusContract.Endpoint "splitUtxO" PABSplitUtxOParams PlutusContract..\/
        PlutusContract.Endpoint "mintFree" PABMintFreeParams PlutusContract..\/

        PlutusContract.Endpoint "prepareProtocol" PABPrepareProtocolParams PlutusContract..\/
        PlutusContract.Endpoint "updateProtocol" PABUpdateProtocolParams PlutusContract..\/
        PlutusContract.Endpoint "sell" PABSellNFTParams PlutusContract..\/
        PlutusContract.Endpoint "buy" PABBuyNFTParams PlutusContract..\/
        PlutusContract.Endpoint "getback" PABGetBackNFTParams 

endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (prepareProtocol' `PlutusContract.select` updateProtocol' `PlutusContract.select` sell' `PlutusContract.select` buy' `PlutusContract.select` getback') >> endpoints
  where
    prepareProtocol' = PlutusContract.endpoint @"prepareProtocol" prepareProtocol
    updateProtocol' = PlutusContract.endpoint @"updateProtocol" updateProtocol
    sell' = PlutusContract.endpoint @"sell" sell
    buy' = PlutusContract.endpoint @"buy" buy
    getback' = PlutusContract.endpoint @"getback" getback

Playground.Contract.mkSchemaDefinitions ''ValidatorSchema

--------------------------------------------------------------------------------

data ValidatorContracts = 

    BalanceAtScript PABBalanceAtScriptParams  |
    SplitUtxO PABSplitUtxOParams  |
    MintFree PABMintFreeParams  |

    PrepareProtocol PABPrepareProtocolParams | 
    UpdateProtocol PABUpdateProtocolParams | 
    SellNFT PABSellNFTParams | 
    BuyNFT PABBuyNFTParams | 
    GetBackNFT PABGetBackNFTParams
    deriving (P.Eq, P.Ord, P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

instance Prettyprinter.Pretty ValidatorContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions ValidatorContracts where

    getDefinitions        = [
        BalanceAtScript exampleBalanceAtScriptParams,
        SplitUtxO exampleSplitUtxOParams,
        MintFree exampleMintFreeParams,

        PrepareProtocol examplePrepareProtocolParams,
        UpdateProtocol exampleUpdateProtocolParams,
        SellNFT exampleSellNFTParams, 
        BuyNFT exampleBuyNFTParams, 
        GetBackNFT exampleGetBackNFTParams
        ]

    getContract (BalanceAtScript bsp) = PABEffectsContractBuiltin.SomeBuiltin $ balanceAtScript @() @PABEffectsContractBuiltin.Empty bsp
    getContract (SplitUtxO sp) = PABEffectsContractBuiltin.SomeBuiltin $ splitUtxO @() @PABEffectsContractBuiltin.Empty sp
    getContract (MintFree mp) = PABEffectsContractBuiltin.SomeBuiltin $ mintFree @() @PABEffectsContractBuiltin.Empty mp

    getContract (PrepareProtocol pp) = PABEffectsContractBuiltin.SomeBuiltin $ prepareProtocol @() @PABEffectsContractBuiltin.Empty pp
    getContract (UpdateProtocol upp) = PABEffectsContractBuiltin.SomeBuiltin $ updateProtocol @() @PABEffectsContractBuiltin.Empty upp
    getContract (SellNFT sp) = PABEffectsContractBuiltin.SomeBuiltin $ sell         @() @PABEffectsContractBuiltin.Empty sp
    getContract (BuyNFT bp)   = PABEffectsContractBuiltin.SomeBuiltin $ buy         @() @PABEffectsContractBuiltin.Empty bp
    getContract (GetBackNFT gbp)  = PABEffectsContractBuiltin.SomeBuiltin $ getback @() @PABEffectsContractBuiltin.Empty gbp

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty

--------------------------------------------------------------------------------
