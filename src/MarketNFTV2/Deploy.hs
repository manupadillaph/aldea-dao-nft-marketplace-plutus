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
module MarketNFTV2.Deploy where
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
import qualified MarketNFTV2.ProtocolIDPolicy as OnChain
import qualified MarketNFTV2.OnChain as OnChain
import qualified MarketNFTV2.OffChain as OffChain
import qualified MarketNFTV2.Types        as T
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


readFileDecodedAsDatumValidatorProtocol :: P.String -> P.IO T.DatumProtocol
readFileDecodedAsDatumValidatorProtocol filepath = do
    raw <- Utils.readFileDecodedAsDatum filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let datumValidator = PlutusTx.unsafeFromBuiltinData @T.DatumProtocol (LedgerApiV2.getDatum raw)
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

-- readFileDecodedAsRedeemerValidatorProtocol "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/redemer.json"
readFileDecodedAsRedeemerValidatorProtocol :: P.String -> P.IO T.RedeemerProtocol
readFileDecodedAsRedeemerValidatorProtocol filepath = do
    raw <- Utils.readFileDecodedAsRedeemer filepath
    P.putStrLn $ "Raw: " ++ P.show raw
    let redeemerValidator = PlutusTx.unsafeFromBuiltinData @T.RedeemerProtocol (LedgerApiV2.getRedeemer raw)
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

writeValidator :: P.String -> P.String -> LedgerApiV2.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
writeValidator path file validator = do
    Utils.writeValidator path (file ++ ".plutus") validator

writeValidatorHash :: P.String -> P.String -> LedgerApiV2.ValidatorHash -> P.IO ()
writeValidatorHash path file hashValidator = do
    Utils.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".hash") hashValidator

writeValidatorAddress :: P.String -> P.String -> LedgerAddress.Address -> P.IO ()
writeValidatorAddress path file address = do
    -- let  address = OnChain.addressValidator
    Utils.writeEncodedToFile (path SystemFilePathPosix.</> file ++ "-HEX.addr") address
    Utils.writeFile (path SystemFilePathPosix.</> file ++ "-Mainnet.addr") $ Utils.validatorAddrToAddrBech32Mainnet address
    Utils.writeFile (path SystemFilePathPosix.</> file ++ "-Testnet.addr") $ Utils.validatorAddrToAddrBech32Testnet address

---------------------------------------------------------------------------------------------------------------

writeMintingPolicy :: LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol -> P.String -> P.String -> P.IO ()
writeMintingPolicy policy curSymbol path file    = do
    _ <- Utils.writeMintingPolicy path (file ++ ".plutus") policy
    Utils.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".symbol") curSymbol

---------------------------------------------------------------------------------------------------------------

getEnvPLUTUS_DEVS_SCRIPTS_FILES :: P.String -> P.IO P.String
getEnvPLUTUS_DEVS_SCRIPTS_FILES version = do
    basePathFilesMaybe <- SystemEnvironment.lookupEnv "PLUTUS_DEVS_SCRIPTS_FILES"
    return $ case basePathFilesMaybe of
        Nothing     -> "files/validators" SystemFilePathPosix.</> version
        Just path -> path SystemFilePathPosix.</> "validators" SystemFilePathPosix.</> version

-----------------------

returnIO :: a -> P.IO a
returnIO = return

-----------------------

-- exportarScriptsFromStringsParams (Just "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/files") Nothing "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "0d74ec8142e06007b3969313e391cf30ad8b2a96efa3293ba161b91f59564d8c#1"

-- exportarScriptsFromStringsParams (Just "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/files") Nothing "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "0d74ec8142e06007b3969313e391cf30ad8b2a96efa3293ba161b91f59564d8c#1"

-- exportarScriptsFromStringsParams (Just "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/files") Nothing "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "3189c00ec7fed2e16e3b872557df7eafe883c0f92fbd095508c7879c4c62edec#1"

-- :set -XOverloadedStrings
-- exportarScriptsFromStringsParams (Just "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/files") ( Just "MarketNFT2") "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "01b8cb79d0fe2c64e9ad8baf608b9216f5e362e83bed3360371390e93b6f0957#0"

-- :set -XOverloadedStrings
-- exportarScriptsFromStringsParams (Just "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/files") ( Just "MarketNFT2") "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "01b8cb79d0fe2c64e9ad8baf608b9216f5e362e83bed3360371390e93b6f0957#1"

exportarScriptsFromStringsParams :: Maybe P.FilePath -> Maybe P.String ->  P.String -> P.String -> P.IO OffChain.PABParams
exportarScriptsFromStringsParams basePathFilesMaybe nombreProtocolMaybe mastersStr uTxOutRefStr   = do

    let 
        basePathFiles = DataMaybe.fromMaybe "export/deploy/" basePathFilesMaybe
        nombreProtocol = DataMaybe.fromMaybe "MarketNFT" nombreProtocolMaybe

    ----------------------------------------
    let protocolID_TxOutRef = Utils.unsafeReadTxOutRef uTxOutRefStr
    P.putStrLn $ "ProtocolID_TxOutRef: " ++ P.show protocolID_TxOutRef

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
    
    exportarScripts basePathFiles nombreProtocol masters protocolID_TxOutRef 

----------------------------------------------------------------------------------------

exportarScripts ::P.FilePath -> P.String ->  [T.WalletPaymentPKH] -> LedgerApiV2.TxOutRef -> P.IO OffChain.PABParams
exportarScripts basePathFiles nombreProtocol masters protocolID_TxOutRef  = do

    SystemDirectory.removePathForcibly (basePathFiles SystemFilePathPosix.</> nombreProtocol )
    SystemDirectory.createDirectoryIfMissing True (basePathFiles SystemFilePathPosix.</> nombreProtocol )
    
    ----------------------------------------
    
    let mintingPolicy = OnChain.policy_ProtocolID protocolID_TxOutRef
    let currencySymbol = Utils.getCurSymbolOfPolicy mintingPolicy

    P.putStrLn "Generating 'ProtocolID' Script Minting"

    _ <- writeMintingPolicy mintingPolicy currencySymbol (basePathFiles SystemFilePathPosix.</> nombreProtocol) "ProtocolID"

    ----------------------------------------

    P.putStrLn "Generating 'Protocol' Validator Script..."

    let validatorProtocol = OnChain.codeValidatorProtocol currencySymbol masters
    let hashValidatorProtocol = UtilsScriptsV2.validatorHash validatorProtocol
    let addressProtocol = Ledger.scriptHashAddress hashValidatorProtocol

    _ <- writeValidator (basePathFiles SystemFilePathPosix.</> nombreProtocol) "Protocol" validatorProtocol
    _ <- writeValidatorHash (basePathFiles SystemFilePathPosix.</> nombreProtocol) "Protocol" hashValidatorProtocol
    _ <- writeValidatorAddress (basePathFiles SystemFilePathPosix.</> nombreProtocol) "Protocol" addressProtocol

    ----------------------------------------

    P.putStrLn "Generating 'Main' Validator Script..."

    let validator = OnChain.codeValidator currencySymbol T.aldea_CS
    let hashValidator = UtilsScriptsV2.validatorHash validator
    let address = Ledger.scriptHashAddress hashValidator

    _ <- writeValidator (basePathFiles SystemFilePathPosix.</> nombreProtocol) "Main" validator
    _ <- writeValidatorHash (basePathFiles SystemFilePathPosix.</> nombreProtocol) "Main" hashValidator
    _ <- writeValidatorAddress (basePathFiles SystemFilePathPosix.</> nombreProtocol) "Main" address

    ----------------------------------------

    let
        pABParams = OffChain.PABParams {

            OffChain.ppProtocolID_TxOutRef = protocolID_TxOutRef,
            OffChain.ppValidatorProtocol = validatorProtocol,
            OffChain.ppValidatorProtocolAddress = addressProtocol,
            OffChain.ppValidatorProtocolHash = hashValidatorProtocol,
            OffChain.ppValidator = validator,
            OffChain.ppValidatorAddress = address,
            OffChain.ppValidatorHash = hashValidator,
            OffChain.ppProtocolID_Policy = mintingPolicy,
            OffChain.ppProtocolID_CS = currencySymbol
        }

    eValidatorProtocol <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "Protocol.plutus") 
    eValidatorProtocolAddressTestnet <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "Protocol-Testnet.addr")
    eValidatorProtocolAddressMainnet <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "Protocol-Mainnet.addr")
    eValidator <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "Main.plutus")
    eValidatorAddressTestnet <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "Main-Testnet.addr")
    eValidatorAddressMainnet <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "Main-Mainnet.addr")
    ePolicy_ProtocolID <- Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "ProtocolID.plutus")

    let    
        extra =  OffChain.PABParamsExtras {
             OffChain.ePABParams = pABParams,
             OffChain.eValidatorProtocol = Utils.lazyByteStringToString eValidatorProtocol,  
             OffChain.eValidatorProtocolAddressTestnet =  Utils.lazyByteStringToString eValidatorProtocolAddressTestnet,
             OffChain.eValidatorProtocolAddressMainnet =  Utils.lazyByteStringToString eValidatorProtocolAddressMainnet,
             OffChain.eValidator =  Utils.lazyByteStringToString eValidator, 
             OffChain.eValidatorAddressTestnet =  Utils.lazyByteStringToString eValidatorAddressTestnet,
             OffChain.eValidatorAddressMainnet =  Utils.lazyByteStringToString eValidatorAddressMainnet,
             OffChain.ePolicy_ProtocolID =  Utils.lazyByteStringToString ePolicy_ProtocolID 
        }
        
    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "PABParams-HEX.json") pABParams

    Utils.writeEncodedToFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "PABParamsExtras-HEX.json") extra

    P.putStrLn $ "Saved PAB Params in: " ++ P.show (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "PABParams-HEX.json")

    return pABParams