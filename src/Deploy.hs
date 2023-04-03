{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- HLINT ignore "Use camelCase" -}
----------------------------------------------------------------------------------------
module Deploy where
----------------------------------------------------------------------------------------
import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Shelley                                 as ApiShelley
--import qualified Cardano.Ledger.Alonzo                             as CardanoLedgerAlonzo (AlonzoEra)
--import qualified Cardano.Ledger.Alonzo.Language            as CardanoLedgerAlonzoLanguage    (Language (..))
--import qualified Cardano.Ledger.Alonzo.Scripts             as CardanoLedgerAlonzoScripts (ExUnits (..), Script (..))
--import qualified Cardano.Crypto.Hash.Class                     as CryptoHashClass (hashToBytes)
--import qualified Cardano.Ledger.BaseTypes                        as LedgerBaseTypes (certIxToInt, txIxToInt)
--import qualified Cardano.Ledger.Credential                     as LedgerCredential
--import qualified Cardano.Ledger.Crypto                             as LedgerCrypto (StandardCrypto)
--import qualified Cardano.Ledger.Hashes                             as LedgerHashes (ScriptHash (..))
--import qualified Cardano.Ledger.Keys                                 as LedgerKeys (KeyHash (..))
import qualified Codec.Serialise                                         as CodecSerialise (serialise)
import qualified Control.Monad.IO.Class                            as MonadIOClass (MonadIO (..))
import qualified Control.Monad                                             as Monad
import qualified Data.Aeson as DataAeson (decode) --, encode
--import qualified Data.ByteString.Lazy                                as DataByteStringLazy
--import qualified Data.ByteString.Short                             as SBS
--import qualified Data.ByteString.Char8                             as DataByteStringChar8
import qualified Data.ByteString.Short                                    as DataByteStringShort
import qualified Data.ByteString.Lazy                                     as DataByteStringLazy
import qualified Data.List.Split                         as DataListSplit
import qualified Data.Maybe as DataMaybe (fromMaybe, fromJust) --,
--import qualified Data.Map                                                        as DataMap
import qualified Data.String as DataString (IsString (fromString))
--import qualified Data.Text                                                     as DataText (pack, Text) --,
import qualified Data.Text.Internal.Search as DataTextSearch
import qualified Ledger
import qualified Ledger.Address                      as LedgerAddress
--import qualified Ledger.Bytes                                                as LedgerBytes (LedgerBytes(LedgerBytes), fromHex)
import qualified Ledger.Value as LedgerValue
--import qualified Network.Curl                                                as NetworkCurl
--import qualified Network.Curl.Aeson                                    as NetworkCurlAeson
import qualified Plutus.Script.Utils.V2.Scripts as UtilsScriptsV2
import qualified Plutus.V1.Ledger.Api as LedgerApiV1
--import qualified Plutus.V1.Ledger.Credential                 as LedgerCredentialV1
--import qualified Plutus.V1.Ledger.Crypto                         as LedgerCryptoV1
--import qualified Plutus.V1.Ledger.EvaluationContext as LedgerEvaluationContextV1
--import qualified Plutus.V1.Ledger.Value                            as LedgerValueV1 (TokenName (..))
import qualified Plutus.V1.Ledger.Scripts as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
--import qualified Plutus.V2.Ledger.Credential                 as LedgerCredentialV2
--import qualified Plutus.V2.Ledger.Crypto                         as LedgerCryptoV2
import qualified Plutus.V2.Ledger.EvaluationContext as LedgerEvaluationContextV2

--import qualified Plutus.V2.Ledger.Scripts                        as LedgerScriptsV2
--import qualified Plutus.V2.Ledger.Value                            as LedgerValueV2 (TokenName (..))
import qualified PlutusTx
--import qualified PlutusTx.Builtins                                            as TxBuiltins (toBuiltin)
import qualified PlutusTx.Builtins.Class                                as TxBuiltinsClass

--import qualified PlutusTx.Builtins.Internal                         as TxBuiltinsInternal (BuiltinByteString (..))
import PlutusTx.Prelude hiding (unless)
import qualified System.Directory as SystemDirectory
import qualified System.Environment as SystemEnvironment (lookupEnv)
import qualified System.FilePath.Posix as SystemFilePathPosix
import qualified Text.Hex                                      as TextHex

import qualified Text.RE.Replace as TextREReplace
import qualified Text.RE.TDFA.String as TextRETDFAString
import qualified Text.Read as TextRead (readMaybe)
--import qualified Wallet.Emulator.Wallet                                 as WalletEmulator            (WalletId (..)) --, Wallet (..)
--import qualified Wallet.Types                                                     as WalletTypes (ContractInstanceId (..))
----------------------------------------------------------------------------------------
import qualified Utils
import qualified Validators.MarketNFTV2.ProtocolIDPolicy as OnChain
import qualified Validators.MarketNFTV2.OnChain as OnChain
import qualified Validators.MarketNFTV2.Types        as T
import qualified Prelude as P
----------------------------------------------------------------------------------------

-- lee archivos exportados con Utils.writeEncodedToFile
-- readFileDecodedAsDatumValidator "/home/manuelpadilla/source/copyRepos/RATS-DAO/cardano-devs-scripts/files/validators/V2/StakePlusV2/StakePlusV2-FundDatum-HEX.json"
-- contents of file: {"getDatum":"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff"}
readFileDecodedAsDatumValidator :: P.String -> P.IO T.DatumMarket
readFileDecodedAsDatumValidator filepath = do
    raw <- Utils.readFileDecodedAsDatum filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let datumValidator = PlutusTx.unsafeFromBuiltinData @T.DatumMarket (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show datumValidator
    return datumValidator

-- readFileDecodedAsRedeemerValidator "/home/manuelpadilla/source/copyRepos/Plutus-Devs/cardano-devs-scripts/files/validators/V2/exampleRedeemerRedeemerBuyerBuyNFT-HEX.json"
-- contents: {"getRedeemer":"d87a80"}
readFileDecodedAsRedeemerValidator :: P.String -> P.IO T.RedeemerMarket
readFileDecodedAsRedeemerValidator filepath = do
    raw <- Utils.readFileDecodedAsRedeemer filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let redeemerValidator = PlutusTx.unsafeFromBuiltinData @T.RedeemerMarket (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show redeemerValidator
    return redeemerValidator

-- lee archivos exportados con Utils.writePlutusDataToFile
-- readFileToPlutusDataAsDatumValidator "/home/manuelpadilla/source/copyRepos/RATS-DAO/cardano-devs-scripts/files/validators/V2/StakePlusV2/StakePlusV2-FundDatum.json"
readFileToPlutusDataAsDatumValidator :: P.String -> P.IO T.DatumMarket
readFileToPlutusDataAsDatumValidator filepath = do
    raw <- Utils.readFileToPlutusData filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let datumValidator = PlutusTx.unsafeFromBuiltinData @T.DatumMarket (LedgerApiV2.BuiltinData raw)
    P.putStrLn $ "Result: " ++ P.show datumValidator
    return datumValidator

-----------------------------------------------------------------------------------------

-- lee strings cbor
-- readEncodedStringAsDatumOrRedemeer "DatumMarket" "{\"getDatum\":\"d8799fd8799f9fd8799f424353444d616e75ffffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "DatumMarket" "{\"getDatum\":\"d87a9fd8799fd8799f424353444d616e75ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc6c01a002dc6c0ffff9fd8799f424353444d616e75ffff1a01c9c38000ffff\"}"
-- readEncodedStringAsDatumOrRedemeer "DatumMarket" "{\"getDatum\":\"d87a9fd8799fd8799f581c9925b87688572c90085d2a6bcaa7e8f4d1e631fc18a4439ea998ce3f5820d793c0edc6b088fbd76e749658a27a7cad242ec4fb63f9547bffccf2bfcbda41ff9fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a002dc96600ffff800000ffff\"}"
-- readEncodedStringAsDatumOrRedemeer "DatumMarket" "{\"getDatum\":\"d87b9fd8799fd8799f424353444d616e75ff581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e1a01c9c3801b0000018212c5d3f81a01c9c3801a01c9c380d8799f1b0000018212c5d3f8ffffff\"}"

-- readEncodedStringAsDatumOrRedemeer "RedeemerMarket" "{\"getRedeemer\":\"d87a9fd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f43aaccffff0affffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "RedeemerNFTTxID" "{\"getRedeemer\":\"d8799fd8799fd8799fd8799f424353444d616e75ff9f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16eff1b0000018212c5d3f89fd8799fd8799f185aff01ffd8799fd8799f18b4ff02ffd8799fd87a8003ffff1a01c9c380424353ffd87a9fd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f43aaccffff0affffffffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "RedeemerNFTFundAndUserID" "{\"getRedeemer\":\"d8799fd8799f40d8799fd8799f43aaafffff00ffffff\"}"
-- readEncodedStringAsDatumOrRedemeer "RedeemerNFTFundAndUserID" "{\"getRedeemer\":\"d87a9fd8799f444d616e75ffff\"}"

-- readEncodedStringAsDatumOrRedemeer "RedeemerNFTTxID" "{\"getRedeemer\":\"d8799fd8799fd8799fd8799f581c47d39eec62a0069c145d691757d2dec20d879a949e0f603b9f3035a346506f6f6c4944ff9f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16eff1b000001835d3453f09fd8799f185a01ffd8799f18b402ffd8799f19270f03ffff1a02faf080581c14f792644b7ce8e294ef44826e6017aee704f9d5f2bfa8b83ce9d38effd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f582025d3b9007a5ab81ef517aea2448dccddd6b494d8404ff000cd23c508e8762c79ff00ffffffffff\"}"

readEncodedStringAsDatumOrRedemeer :: P.String -> P.String -> P.IO ()
readEncodedStringAsDatumOrRedemeer typeToDecode stringCbor = do
    case typeToDecode of
        "DatumMarket" -> do
            raw <- Utils.readStringDecodedAsDatum stringCbor
            P.putStrLn $ "Raw: " ++ P.show raw
            let result = PlutusTx.unsafeFromBuiltinData @T.DatumMarket (LedgerApiV2.getDatum raw)
            P.putStrLn $ "Result: " ++ P.show result
        _ -> do
            raw <- Utils.readStringDecodedAsRedeemer stringCbor
            P.putStrLn $ "Raw: " ++ P.show raw
            case typeToDecode of
                "RedeemerMarket" -> do
                    let result = PlutusTx.unsafeFromBuiltinData @T.RedeemerMarket (LedgerApiV2.getRedeemer raw)
                    P.putStrLn $ "Result: " ++ P.show result
                _ -> P.error "Invalid Type"

    return ()

----------------------------------------------------------------------------------------

writeValidatorV2 :: P.String -> P.String -> LedgerApiV2.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidatorV2 path file validator = do
    Utils.writeValidatorV2 path (file ++ ".plutus") validator

writeValidatorV2Hash :: P.String -> P.String -> LedgerApiV2.ValidatorHash -> P.IO ()
writeValidatorV2Hash path file hashValidator = do
    Utils.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".hash") hashValidator

writeValidatorV2Address :: P.String -> P.String -> LedgerAddress.Address -> P.IO ()
writeValidatorV2Address path file address = do
    -- let  address = OnChain.addressValidator
    Utils.writeEncodedToFile (path SystemFilePathPosix.</> file ++ "-HEX.addr") address
    Utils.writeFile (path SystemFilePathPosix.</> file ++ "-Mainnet.addr") $ Utils.validatorAddrToAddrBech32Mainnet address
    Utils.writeFile (path SystemFilePathPosix.</> file ++ "-Testnet.addr") $ Utils.validatorAddrToAddrBech32Testnet address

---------------------------------------------------------------------------------------------------------------

getScriptShortBs :: LedgerApiV2.Script -> DataByteStringShort.ShortByteString
getScriptShortBs = DataByteStringShort.toShort . DataByteStringLazy.toStrict . CodecSerialise.serialise

getScriptSerialised :: DataByteStringShort.ShortByteString -> ApiShelley.PlutusScript ApiShelley.PlutusScriptV2
getScriptSerialised = ApiShelley.PlutusScriptSerialised


getScriptMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.Script
getScriptMintingPolicy    = LedgerApiV2.getMintingPolicy

writeMintingPolicy :: P.String -> P.String -> LedgerApiV2.MintingPolicy -> P.IO (Either (CardanoApi.FileError ()) ())
writeMintingPolicy path file policy = do
    let
        scriptMintingPolicyV2 = getScriptMintingPolicy policy
        scriptShortBsV2 = getScriptShortBs scriptMintingPolicyV2
        scriptSerialisedV2 = getScriptSerialised scriptShortBsV2
    SystemDirectory.createDirectoryIfMissing True path --SystemFilePathPosix.</> v1dir
    CardanoApi.writeFileTextEnvelope (path SystemFilePathPosix.</> file) Nothing scriptSerialisedV2

---------------------------------------------------------------------------------------------------------------

-- evaluateScriptV2 :: P.IO ()
-- evaluateScriptV2 = do
--     let codeValidator = OnChain.codeValidator
--     Utils.evaluateScriptV2 codeValidator

----------------------------------------------------------------------------------------

getEnvPLUTUS_DEVS_SCRIPTS_FILES :: P.String ->    P.IO P.String
getEnvPLUTUS_DEVS_SCRIPTS_FILES version = do
    basePathFilesMaybe <- SystemEnvironment.lookupEnv "PLUTUS_DEVS_SCRIPTS_FILES"
    return $ case basePathFilesMaybe of
        Nothing     -> "files/validators" SystemFilePathPosix.</> version
        Just path -> path SystemFilePathPosix.</> "validators" SystemFilePathPosix.</> version

returnIO :: a ->    P.IO a
returnIO = return

-----------------------

exportarScripts :: Maybe P.FilePath -> P.String -> P.String -> P.IO ()
exportarScripts basePathFilesMaybe mastersStr uTxOutRefStr = do
    let
        version = "V2"
        nombreScript = "MarketNFT"

    basePathFiles <- case basePathFilesMaybe of
        Nothing -> getEnvPLUTUS_DEVS_SCRIPTS_FILES version
        Just path -> returnIO path

    SystemDirectory.createDirectoryIfMissing True basePathFiles

    _ <- P.putStrLn $ "Guardando en :" ++ P.show    basePathFiles

    ----------------------------------------

    let 
        mastersString = DataListSplit.splitOn "," mastersStr

        getMaster_Str_Ok masterStr' =
            if length masterStr' P.== 56 then
                 masterStr'
            else
                P.error $ "Invalid Master Lenght: " ++ P.show masterStr'

        mastersStrOK = getMaster_Str_Ok <$> mastersString

        mastersHex = TextHex.decodeHex . Utils.stringToStrictText <$> mastersStrOK

        isHexOk hex =
            case hex of
                    Nothing -> False
                    _             -> True

        getMaster_Hex_Ok masterHex' =
            if isHexOk masterHex' then
                DataMaybe.fromJust masterHex'
            else
                P.error $ "Invalid Master Hex: " ++ P.show masterHex'

        mastersHexOK = getMaster_Hex_Ok <$> mastersHex
        masters =    LedgerApiV2.PubKeyHash . TxBuiltinsClass.toBuiltin <$> mastersHexOK

    P.putStrLn $ "Masters: " ++ P.show masters
    ----------------------------------------

    let protocolID_TxOutRef = Utils.unsafeReadTxOutRef uTxOutRefStr
    let mintingPolicy = OnChain.policy_ProtocolID protocolID_TxOutRef
    let currencySymbol = Utils.getCurSymbolOfPolicy mintingPolicy

    P.putStrLn "Exportando Minting Policy"

    _ <- writeMintingPolicy basePathFiles (nombreScript++"-ProtocolID"++ ".plutus") mintingPolicy
    _ <- Utils.writePlutusDataToFile (basePathFiles SystemFilePathPosix.</>  (nombreScript++"-ProtocolID") ++ ".symbol") currencySymbol

    ----------------------------------------

    P.putStrLn "Exportando Validador Principal"

    let validatorProtocol = OnChain.codeValidatorProtocol currencySymbol masters
    let hashValidatorProtocol = UtilsScriptsV2.validatorHash validatorProtocol
    let addressProtocol = Ledger.scriptHashAddress hashValidatorProtocol

    _ <- writeValidatorV2 basePathFiles (nombreScript++"-Protocol") validatorProtocol 
    _ <- writeValidatorV2Hash basePathFiles (nombreScript++"-Protocol") hashValidatorProtocol
    _ <- writeValidatorV2Address basePathFiles (nombreScript++"-Protocol") addressProtocol

    ----------------------------------------

    P.putStrLn "Exportando Validador Principal"

    let validator = OnChain.codeValidator currencySymbol
    let hashValidator = UtilsScriptsV2.validatorHash validator
    let address = Ledger.scriptHashAddress hashValidator

    _ <- writeValidatorV2 basePathFiles (nombreScript++"-Main") validator 
    _ <- writeValidatorV2Hash basePathFiles (nombreScript++"-Main") hashValidator
    _ <- writeValidatorV2Address basePathFiles (nombreScript++"-Main") address

    P.putStrLn "Press return to continue..."

----------------------------------------------------------------------------------------

exportExampleDataV2 :: Maybe P.FilePath -> P.IO ()
exportExampleDataV2 basePathFilesMaybe = do

    let
	version = "V2"
	nombreScript = "MarketNFT"

    basePathFiles <- case basePathFilesMaybe of
		Nothing     -> getEnvPLUTUS_DEVS_SCRIPTS_FILES version
		Just path -> returnIO    path

    SystemDirectory.createDirectoryIfMissing True basePathFiles

    _ <- P.putStrLn $ "Guardando en :" ++ P.show    basePathFiles

    ----------------------------------------

    P.putStrLn "Exportando examples redeemers "

    let exampleRedeemerSellerGetBackNFT = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData T.RedeemerSellerGetBackNFT

    Utils.writePlutusDataToFile (basePathFiles SystemFilePathPosix.</> "exampleRedeemerSellerGetBackNFT.json") exampleRedeemerSellerGetBackNFT
    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> "exampleRedeemerSellerGetBackNFT-HEX.json") exampleRedeemerSellerGetBackNFT

    let exampleRedeemerRedeemerBuyerBuyNFT = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData T.RedeemerBuyerBuyNFT

    Utils.writePlutusDataToFile (basePathFiles SystemFilePathPosix.</> "exampleRedeemerRedeemerBuyerBuyNFT.json") exampleRedeemerRedeemerBuyerBuyNFT
    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> "exampleRedeemerRedeemerBuyerBuyNFT-HEX.json") exampleRedeemerRedeemerBuyerBuyNFT

    ----------------------------------------

    P.putStrLn "Exportando examples datum "

    let


	exampleCurrencySymbol :: LedgerApiV2.CurrencySymbol
	exampleCurrencySymbol = LedgerApiV2.CurrencySymbol "CS"

	exampleTokenName :: LedgerApiV2.TokenName
	exampleTokenName = LedgerApiV2.TokenName "Manu"

	exampleNFT :: T.NFT
	exampleNFT = LedgerValue.assetClass exampleCurrencySymbol exampleTokenName

	exampleSeller :: Ledger.PubKeyHash
	exampleSeller = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

	exampleDatumValidator = T.DatumMarket
	    {
		    T.dSeller = exampleSeller,
		    T.dNFT = exampleNFT,
		    T.dPriceADA = Just 5_000_000,
            T.dPriceALDEA = Nothing
	    }

	exampleDatum = LedgerApiV2.Datum $ PlutusTx.toBuiltinData exampleDatumValidator

    Utils.writePlutusDataToFile (basePathFiles SystemFilePathPosix.</> "exampleDatum.json") exampleDatum
    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> "exampleDatum-HEX.json") exampleDatum

    ----------------------------------------

    P.putStrLn "Hecho!"

    P.putStrLn "Press return to continue..."

    _ <-P.getLine

    return ()

----------------------------------------------------------------------------------------
