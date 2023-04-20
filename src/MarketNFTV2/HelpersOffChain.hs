

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
{- HLINT ignore "Use camelCase" -}
--------------------------------------------------------------------------------
module MarketNFTV2.HelpersOffChain where
--------------------------------------------------------------------------------
import qualified Control.Lens                        as ControlLens
-- import qualified Control.Monad                       as Monad (void)
-- import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
-- import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Map                            as DataMap
-- import qualified Data.Maybe                           as DataMaybe
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
-- import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                             
-- import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Address                      as LedgerAddress
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.TxConstraints                       as LedgerTxConstraints
-- import qualified Ledger.Constraints.ValidityInterval                    as LedgerValidityInterval 
-- import qualified Ledger.Interval                                        as LedgerInterval
import qualified Ledger.Tx                           as LedgerTx (datumInDatumFromQuery, decoratedTxOutDatum, DecoratedTxOut, decoratedTxOutValue)

import qualified Ledger.Value                        as LedgerValue
-- import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1 (RedeemerType, DatumType) -- TypedValidator, ValidatorTypes, mkTypedValidator, mkUntypedValidator, validatorAddress, validatorHash, validatorScript
import qualified Plutus.V1.Ledger.Interval                              as LedgerIntervalV1 (Interval) --from, contains, interval
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2

-- import qualified PlutusTx
import qualified PlutusTx.AssocMap                                 as TxAssocMap
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
-- import qualified Schema                              
-- import qualified Text.Printf                         as TextPrintf (printf)
--------------------------------------------------------------------------------
import qualified MarketNFTV2.Types        as T 
import qualified MarketNFTV2.Helpers        as Helpers
import qualified Text.Printf as TextPrintf
----------------------------------

getDatumFromDecoratedTxOut :: LedgerApiV2.FromData d => LedgerTx.DecoratedTxOut -> Maybe d
getDatumFromDecoratedTxOut decoratedTxOut = do

    (_, mdatum) <- decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
    LedgerApiV2.Datum d <- mdatum ControlLens.^? LedgerTx.datumInDatumFromQuery
    
    case LedgerApiV2.fromBuiltinData d of    
        Nothing -> Nothing
        datum -> datum

getValueFromDecoratedTxOut :: LedgerTx.DecoratedTxOut -> LedgerValue.Value
getValueFromDecoratedTxOut decoratedTxOut =
    let
        (Just value) = decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutValue
    in
        value

findUtxoInValidatorWithNFT :: LedgerAddress.Address -> T.NFT -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
findUtxoInValidatorWithNFT addressValidator nft = do
    utxos <- PlutusContract.utxosAt addressValidator
    let
        xs = [(oref, o) |(oref, o) <- DataMap.toList utxos, LedgerValue.assetClassValueOf (getValueFromDecoratedTxOut o) nft == 1]
    case xs of
        [x] -> return $ Just x   
        _   -> return Nothing

calculateMinAda :: Integer -> Integer -> Integer -> Bool -> Integer
calculateMinAda numAssets sumAssetNameLengths numPIDs isHash =
    let
        -- const numPIDs=1
        -- The number of policy scripts referenced in the UTxO. If there is only one type of token in the UTxO, then this is just 1.
        -- var numAssets=1
        -- The number of asset names present in the UTxO. If there is only one type of token, then this is just 1.
        -- const sumAssetNameLengths=32
        -- Bytes    The number of bytes needed to store all of the asset names. If these do not include unicode characters (e.g., emojis), then this is just the total number of letters (characters) in the asset names. For instance, a token named "Test" needs 4 bytes for its name.

        --Fixed parameters
        !minUTxOValue  =1000000 :: Integer
        --ADA	The minimum number of ADA that must be present in ADA-only UTxOs.
        !pidSize=28
        --Bytes	The number of bytes in a policy ID.
        !coinSize=2
        --Bytes	At the Alonzo HFC, this parameter was corrected to be 2 because the original value 0 was an implementation error.
        !uTxOEntrySizeWithoutVal=27
        --Bytes	The number of bytes in a transaction if there were no value at all in it.
        !adaOnlyUTxOSize=uTxOEntrySizeWithoutVal + coinSize
        --Bytes	The number of bytes in a transaction if it were to only contain ADA.
        -- !coinsPerUTxOWord = TxRatio.truncate $ TxRatio.unsafeRatio minUTxOValue adaOnlyUTxOSize -- = 34482 
        !coinsPerUTxOWord = minUTxOValue `divide` adaOnlyUTxOSize -- = 34482 
        -- coinsPerUTxOByte =  TxRatio.truncate $ TxRatio.unsafeRatio coinsPerUTxOWord  8 -- = 4310.25

        !hash = if isHash then (10 :: Integer) else 0 --si hay data hash suman 10 words

        roundupBytesToWords :: Integer -> Integer
        -- roundupBytesToWords number = TxRatio.truncate ( TxRatio.unsafeRatio (number + 7)  8)
        roundupBytesToWords number = (number + 7) `divide` 8

        !sizeWords = 6 + roundupBytesToWords (numAssets * 12 + sumAssetNameLengths + numPIDs * pidSize )

        !sizeCoins = coinsPerUTxOWord * (uTxOEntrySizeWithoutVal + sizeWords + hash)
        --sizeCoins =  coinsPerUTxOByte * (160 + (sizeWords + hash )* 8 ) 

        !minAda =  if minUTxOValue > sizeCoins then minUTxOValue else sizeCoins
        -- minAda =  if minUTxOValue > sizeCoins then minUTxOValue else sizeCoins
    in
        -- TxRatio.truncate (TxRatio.unsafeRatio (130*minAda) 100) -- 130% of the minimum UTxO value
        (130*minAda) `divide` 100 -- 130% of the minimum UTxO value


calculateMinAdaOfValue :: LedgerApiV2.Value -> Bool -> Integer
calculateMinAdaOfValue value isHash =
    let
        -- !valueWithOutAda = value <> negate ( LedgerValue.adaOnlyValue value)
        !valueWithOutAda = value
        !flattenValue' = Helpers.flattenValue valueWithOutAda
        -- por que siempre esta el pid de ada, no lo pude eliminar ni con la resta anterior (valueWithOutAda)
        -- al hacer LedgerValue.Value mp, el mapa pm contiene de nuevo el pid de ada, con 0 amount.
        sumarPId (LedgerValue.Value mp) = length (TxAssocMap.toList mp) - 1
        !numPIDs = sumarPId valueWithOutAda
        !numAssets = length [ tn | (_, tn, amt) <- flattenValue' , amt > 0 ]
        !sumAssetNameLengths = sum [ lengthOfByteString  $ LedgerApiV2.unTokenName tn | (_, tn, amt) <- flattenValue' , amt > 0 ]

        !minAda =  calculateMinAda numAssets sumAssetNameLengths numPIDs isHash
    in
        minAda 


mintNFT_With_TxOut :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut
                                 -> LedgerApiV2.MintingPolicy
                                 -> LedgerApiV2.TxOutRef
                                 -> Maybe LedgerApiV2.Redeemer
                                 -> LedgerValue.Value
                                 -- -> LedgerValidityInterval.ValidityInterval LedgerApiV2.POSIXTime
                                 -> LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
                                 -> Ledger.PaymentPubKeyHash
                                 -> (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mintNFT_With_TxOut uTxOs policy txOutRef redeemerMint valueForMint validityRange pPKH = do
    let
        use = head [(t,ci) | (t,ci) <- DataMap.toList uTxOs, t == txOutRef]
        --map = zip (fst use) (snd use)
        mapz = DataMap.fromList [use]

        lookupsTxMint =
            -- This script is goint to use see all the uTxO from master or user walllet
            -- LedgerConstraints.unspentOutputs uTxOs P.<> 
            LedgerConstraints.unspentOutputs mapz  P.<>
            -- Is going to Mint the NFT
            LedgerConstraints.plutusV2MintingPolicy policy

        txMint =
            -- Is going to spend the user uTxO assinged to the TxID
            LedgerConstraints.mustSpendPubKeyOutput txOutRef P.<>
            -- Is going to Mint the NFT
            case redeemerMint of
                Nothing -> LedgerConstraints.mustMintValue valueForMint
                Just r  -> LedgerConstraints.mustMintValueWithRedeemer r valueForMint
            P.<>
            LedgerConstraints.mustValidateIn validityRange           P.<>   

            -- LedgerConstraints.mustValidateInTimeRange validityRange P.<>
            LedgerConstraints.mustBeSignedBy pPKH

    (lookupsTxMint, txMint)

------------------------------------------------------------------------------------------

handleContractError :: DataText.Text -> PlutusContract.Contract w s DataText.Text ()
handleContractError err = do
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------- ERROR ----------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "%s" (P.show err)
    -- PlutusContract.logError $ "Caught error: " ++ DataText.unpack err
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------------------------------------------------------------------------"
    
    -- entonces dentro de un contrato deberia poner esto antes de todo y el handlker se ocuparia del error
    -- ControlMonadErrorLens.handling PlutusContract._ContractError handleContractError $ do
    -- PlutusContract.handleError (\err -> PlutusContract.logError $ "Caught error: " ++ DataText.unpack err) $ do
    -- PlutusContract.handleError handleContractError $ do

