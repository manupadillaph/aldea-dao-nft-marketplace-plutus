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
module Validators.MarketNFTV2.OffChain
    (
        SellNFTParams (..), BuyNFTParams (..), GetBackNFTParams (..),
        ValidatorSchema,
        sell, buy, getback,
        endpoints 
    ) where
--------------------------------------------------------------------------------
import qualified Control.Lens                        as ControlLens
import qualified Control.Monad                       as Monad (void)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Map                            as DataMap
import qualified Data.Maybe                           as DataMaybe
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger                              (getCardanoTxId, pubKeyHashAddress, unPaymentPubKeyHash, PaymentPubKeyHash(..) ) 
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Address                      as LedgerAddress
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Tx                           as LedgerTx (datumInDatumFromQuery, decoratedTxOutDatum, DecoratedTxOut, decoratedTxOutValue)
import qualified Ledger.Value                        as LedgerValue
import qualified Playground.Contract                 (mkSchemaDefinitions)
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V1.Ledger.Interval           as LedgerIntervalV1 (interval) 
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap                                 as TxAssocMap
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              
import qualified Text.Printf                         as TextPrintf (printf)
--------------------------------------------------------------------------------
import qualified Validators.MarketNFTV2.Types        as T (DatumMarket  (..), RedeemerMarket (..), NFT)
import qualified Validators.MarketNFTV2.OnChain      as OnChain
--------------------------------------------------------------------------------

instance Schema.ToSchema LedgerApiV2.Validator where
  toSchema = Schema.FormSchemaUnit

-- TODO: para cuando vuelva a usar plutus-1.1.0, tengo que desactivar esto
instance Schema.ToSchema  LedgerAddress.Address where
  toSchema = Schema.FormSchemaUnit

instance Schema.ToSchema   LedgerApiV2.MintingPolicy where
  toSchema = Schema.FormSchemaUnit


data PABParams = PABParams
    {
        ppProtocolID_TxOutRef :: LedgerApiV2.TxOutRef,
        ppValidatorProtocol :: LedgerApiV2.Validator,
        ppValidatorProtocolAddress :: LedgerAddress.Address,
        ppValidatorProtocolHash :: LedgerApiV2.ValidatorHash,
        ppValidator :: LedgerApiV2.Validator,
        ppValidatorAddress :: LedgerAddress.Address,
        ppValidatorHash :: LedgerApiV2.ValidatorHash,
        ppPolicy_ProtocolID :: LedgerApiV2.MintingPolicy,
        ppCurSymbol_ProtocolID :: LedgerApiV2.CurrencySymbol

    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)


data SellNFTParams = SellNFTParams
    {
        spParams :: PABParams,
        spNFT   :: T.NFT,
        spPrice :: Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data BuyNFTParams = BuyNFTParams
    {
        bpParams :: PABParams,
        bpNFT   :: T.NFT,
        bpPrice :: Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

data GetBackNFTParams = GetBackNFTParams
    {
        gbpParams :: PABParams,
        gbpNFT   :: T.NFT
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema, P.Show)

type ValidatorSchema =
        PlutusContract.Endpoint "sell" SellNFTParams PlutusContract..\/
        PlutusContract.Endpoint "buy" BuyNFTParams PlutusContract..\/
        PlutusContract.Endpoint "getback" GetBackNFTParams

sell :: SellNFTParams -> PlutusContract.Contract w s DataText.Text ()
sell SellNFTParams{..} = do

    let hashValidator = ppValidatorHash spParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    sellerPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let sellerPkh = Ledger.unPaymentPubKeyHash sellerPPKH

    let walletAdds = Ledger.pubKeyHashAddress sellerPPKH Nothing

    utxosAtWallet <- PlutusContract.utxosAt walletAdds

    let datum = T.DatumMarket
            {
                dSeller = sellerPkh,
                dNFT = spNFT,
                dPriceADA = Just spPrice,
                dPriceALDEA = Nothing
            }

        (cs, tn) = LedgerValue.unAssetClass spNFT

        value = LedgerValue.singleton cs tn 1 
        minAda = calculateMinAdaOfValue value True
        valuePlusAda = value <> LedgerAda.lovelaceValueOf minAda

        validTimeRange :: LedgerApiV2.POSIXTime
        !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
        !intervalOffset1 = 1000
        !intervalOffset2 = validTimeRange - 1000
        !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
        
        lookupsTx = 
            LedgerConstraints.unspentOutputs utxosAtWallet 

        tx = 
            LedgerConstraints.mustPayToOtherScriptWithDatumInTx hashValidator (LedgerApiV2.Datum $ PlutusTx.toBuiltinData datum) valuePlusAda  P.<>
            LedgerConstraints.mustValidateIn validityRange           

    ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookupsTx tx

    Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx

    PlutusContract.logInfo @P.String $ TextPrintf.printf  "--------------------------- Sell Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show datum) (P.show valuePlusAda)

buy :: forall w s. BuyNFTParams ->  PlutusContract.Contract w s DataText.Text ()
buy BuyNFTParams{..} = do

    let codeValidator = ppValidator bpParams
    let addressValidator = ppValidatorAddress bpParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    buyerPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let buyerPkh = Ledger.unPaymentPubKeyHash buyerPPKH

    let
        buyerAdds = Ledger.pubKeyHashAddress buyerPPKH Nothing

    utxosAtBuyer <- PlutusContract.utxosAt buyerAdds 
    
    utxo <- findUtxoInValidatorWithNFT addressValidator bpNFT 

    case utxo of
            
            Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Buy Endpoint - Error - No Encontré NFT ---------------------------"
            
            Just (oref, decoratedTxOut) -> do

                PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Buy Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)
                
                let

                    Just dOld = getDatumFromDecoratedTxOut decoratedTxOut

                    (cs, tn) = LedgerValue.unAssetClass bpNFT

                    vGetNFT     = LedgerValue.singleton cs tn 1 
                    minAda = calculateMinAdaOfValue vGetNFT True
                    vGetNFTPlusAda = vGetNFT <> LedgerAda.lovelaceValueOf minAda
                    
                    -- TODO: Verificar si el valor de ADA es Just o Nothing, si no usar el valor en ALDEA
                    vGetADA     = LedgerAda.lovelaceValueOf $ DataMaybe.fromJust $ T.dPriceADA dOld

                    redeemer = T.RedeemerBuyerBuyNFT
                    redeemerLedger  = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

                    validTimeRange :: LedgerApiV2.POSIXTime
                    !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
                    !intervalOffset1 = 1000
                    !intervalOffset2 = validTimeRange - 1000
                    !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)

                    lookups =
                        LedgerConstraints.plutusV2OtherScript codeValidator P.<>
                        LedgerConstraints.unspentOutputs (DataMap.singleton oref decoratedTxOut) P.<>
                        LedgerConstraints.unspentOutputs utxosAtBuyer

                    tx =
                        LedgerConstraints.mustSpendScriptOutput oref redeemerLedger P.<>
                        LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash $ T.dSeller dOld) vGetADA  P.<>
                        LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash buyerPkh) vGetNFTPlusAda  P.<>
                        LedgerConstraints.mustValidateIn validityRange            

                --PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Buy Endpoint ---------------------------" 
                ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void lookups tx
                Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
                PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Buy EndPoint - Submited -------------------------"


getback :: forall w s. GetBackNFTParams ->  PlutusContract.Contract w s DataText.Text ()
getback GetBackNFTParams{..} = do


    let codeValidator = ppValidator gbpParams
    let addressValidator = ppValidatorAddress gbpParams

    (now,_) <- PlutusContract.currentNodeClientTimeRange

    sellerPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let sellerPkh = Ledger.unPaymentPubKeyHash sellerPPKH

    let
        sellerAdds = Ledger.pubKeyHashAddress sellerPPKH Nothing

    utxosAtseller <- PlutusContract.utxosAt sellerAdds

    utxo <- findUtxoInValidatorWithNFT addressValidator gbpNFT

    case utxo of

        Nothing ->PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack Endpoint - Error - No Encontré NFT ---------------------------"

        Just (oref, decoratedTxOut) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack Endpoint - Redeem Utxo: %s ---------------------------" (P.show oref)

            let

                --Just dOld = getDatumFromDecoratedTxOut decoratedTxOut

                (cs, tn) = LedgerValue.unAssetClass gbpNFT

                vGetNFT     = LedgerValue.singleton cs tn 1 
                minAda = calculateMinAdaOfValue vGetNFT True
                vGetNFTPlusAda = vGetNFT <> LedgerAda.lovelaceValueOf minAda


                redeemer = T.RedeemerSellerGetBackNFT
                redeemerLedger  = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData redeemer

                validTimeRange :: LedgerApiV2.POSIXTime
                !validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos
                !intervalOffset1 = 1000
                !intervalOffset2 = validTimeRange - 1000
                !validityRange   = LedgerIntervalV1.interval ( now - intervalOffset1 ) (now + intervalOffset2)
                
                lookups =
                    LedgerConstraints.plutusV2OtherScript codeValidator P.<>
                    LedgerConstraints.unspentOutputs (DataMap.singleton oref decoratedTxOut) P.<>
                    LedgerConstraints.unspentOutputs utxosAtseller

                tx =
                    LedgerConstraints.mustSpendScriptOutput oref redeemerLedger P.<>
                    LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash sellerPkh) vGetNFTPlusAda P.<>
                    LedgerConstraints.mustValidateIn validityRange          

            --PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack EndPoint ---------------------------" 
            ledgerTx <- PlutusContract.submitTxConstraintsWith @DataVoid.Void  lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
            PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- GetBack EndPoint - Submited -------------------------"

getDatumFromDecoratedTxOut :: LedgerTx.DecoratedTxOut -> Maybe T.DatumMarket
getDatumFromDecoratedTxOut decoratedTxOut = do

    (_, mdatum) <- decoratedTxOut ControlLens.^? LedgerTx.decoratedTxOutDatum
    LedgerApiV2.Datum d <- mdatum ControlLens.^? LedgerTx.datumInDatumFromQuery
    
    case (LedgerApiV2.fromBuiltinData d :: Maybe T.DatumMarket) of    
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

flattenValue :: LedgerValue.Value -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
flattenValue (LedgerValue.Value mp) =
    let
        !f1 = TxAssocMap.toList mp
        !f2 = [ ( cs , TxAssocMap.toList mp') | (cs, mp')  <- f1 ]
        !f3 = [ (cs , tn, amt) | (cs, f4) <- f2, (tn, amt) <- f4 ]
    in
        f3

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
        !flattenValue' = flattenValue valueWithOutAda
        -- por que siempre esta el pid de ada, no lo pude eliminar ni con la resta anterior (valueWithOutAda)
        -- al hacer LedgerValue.Value mp, el mapa pm contiene de nuevo el pid de ada, con 0 amount.
        sumarPId (LedgerValue.Value mp) = length (TxAssocMap.toList mp) - 1
        !numPIDs = sumarPId valueWithOutAda
        !numAssets = length [ tn | (_, tn, amt) <- flattenValue' , amt > 0 ]
        !sumAssetNameLengths = sum [ lengthOfByteString  $ LedgerApiV2.unTokenName tn | (_, tn, amt) <- flattenValue' , amt > 0 ]

        !minAda =  calculateMinAda numAssets sumAssetNameLengths numPIDs isHash
    in
        minAda 
-- checkUTXO  :: (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) -> Ledger.PaymentPubKeyHash -> Integer -> Bool
-- checkUTXO (oref,o)  ppkh name = do
--     case getDatumFromDecoratedTxOut (oref,o) of
--         Nothing -> False
--         Just T.DatumMarket{..}
--             | T.aCreator dData == ppkh && T.aName dData == name -> True
--             | otherwise                                                              -> False

-- findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)]  -> Ledger.PaymentPubKeyHash -> Integer -> Maybe LedgerApiV2.TxOutRef
-- findUTXO [] _ _ = Nothing --do  
-- findUTXO [(oref,o)]  ppkh name  = do
--     if checkUTXO (oref, o) ppkh name then
--         return oref
--     else
--         Nothing
-- findUTXO ((oref,o):xs) ppkh name
--     | checkUTXO (oref ,o)  ppkh name = return oref
--     | otherwise = findUTXO xs   ppkh name


-- getTxOutRefAndDecoratedTxOutFromTxOutRef :: LedgerApiV2.TxOutRef -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut)
-- getTxOutRefAndDecoratedTxOutFromTxOutRef get_oref= do
--     utxos <- PlutusContract.utxosAt OnChain.addressValidator
--     let
--         xs = [ (oref, o) | (oref, o) <- DataMap.toList utxos , get_oref == oref]
--     case xs of
--         [x] ->  return x

endpoints :: PlutusContract.Contract () ValidatorSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (sell' `PlutusContract.select` buy' `PlutusContract.select` getback') >> endpoints
  where
    sell' = PlutusContract.endpoint @"sell" sell
    buy' = PlutusContract.endpoint @"buy" buy
    getback' = PlutusContract.endpoint @"getback" getback

Playground.Contract.mkSchemaDefinitions ''ValidatorSchema



