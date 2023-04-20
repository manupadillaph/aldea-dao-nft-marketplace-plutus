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
{-# LANGUAGE ConstraintKinds        #-}

{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
module MarketNFTV2.PABSimulator where
--------------------------------------------------------------------------------
-- import qualified Cardano.Node.Emulator.TimeSlot                                             as CardanoNodeEmulatorTimeSlot
import qualified Control.Concurrent.STM                                                     as ConcurrentSTM (atomically)
import qualified Control.Monad.IO.Class                                                     as MonadIOClass (MonadIO (..))
import qualified Control.Monad                                                              as Monad (void)
-- import qualified Control.Monad.Freer                                                     as MonadFreer (interpret)
import qualified Control.Monad.Freer.Internal                                               as MonadFreerInternal (Eff)
import qualified Data.Aeson                                                                 as DataAeson (decode)
-- import qualified Data.ByteString                                                            as DataByteString
import qualified Data.Default                                                               as DataDefault (def)
-- import qualified Data.Fixed                                                              as DataFixed (Pico, Fixed ( MkFixed ))
import qualified Data.List                                                                  as DataList
-- import qualified Data.Map                                                                as DataMap
-- import qualified Data.Maybe                                                                 as DataMaybe (fromJust) --,fromMaybe,
-- import qualified Data.String                                                                as DataString (IsString (fromString))
-- import qualified Data.Text                                                               as DataText (pack, Text)
-- import qualified Data.Text.Internal.Search                                                  as DataTextSearch
-- import qualified Data.Time.Clock                                                         as DataTimeClock (secondsToNominalDiffTime)
-- import qualified Data.Time.Clock.POSIX                                                   as DataTimeClockPOSIX (posixSecondsToUTCTime)
-- import qualified Data.Time.Format                                                        as DataTimeFormat (defaultTimeLocale, formatTime)
import qualified Ledger
import qualified Ledger.Tx                                                                 as LedgerTx
-- import qualified Ledger.Ada                                                                 as LedgerAda
-- import qualified Ledger.Address                                                          as LedgerAddress (Address)
import qualified Ledger.Blockchain                                                          as LedgerBlockchain (value)
-- import qualified Ledger.CardanoWallet                                                       as LedgerCardanoWallet
import qualified Ledger.TimeSlot                                                            as LedgerTimeSlot
import qualified Ledger.Value                                                               as LedgerValue
-- import qualified Playground.Contract                                                     as PlaygroundContract (IO)
import qualified Prelude                                                                    as P
import qualified Plutus.PAB.Core                                                            as PABCore (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin                                        as PABEffectsContractBuiltin (Builtin)
import qualified Plutus.PAB.Simulator                                                       as PABSimulator
import qualified Plutus.PAB.Webserver.Server                                                as PABServer
-- import qualified Plutus.V1.Ledger.Api                                                    as LedgerApiV1
-- import qualified Plutus.V1.Ledger.Bytes                                                  as LedgerBytesV1
-- import qualified Plutus.V2.Ledger.Address                                                as LedgerAddressV2
import qualified Plutus.V2.Ledger.Api                                                       as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Value                                                  as LedgerValueV2
-- import qualified Plutus.V2.Ledger.Tx                                                     as LedgerTxV2 (txOutDatum)
-- import qualified PlutusTx.Builtins.Class                                                    as TxBuiltinsClass
-- import qualified PlutusTx.Builtins.Internal                                              as TxBuiltinsInternal hiding (head, consByteString)
-- import qualified PlutusTx.Eq                                                             as PlutusTxEq
import           PlutusTx.Prelude                                                           hiding (unless)
import qualified System.Directory                                                           as SystemDirectory
import qualified System.Environment                                                         as SystemEnvironment (lookupEnv)
import qualified System.FilePath.Posix                                                      as SystemFilePathPosix
import qualified Text.Read                                                                  as TextRead (readMaybe)
-- import qualified Text.RE.Replace                                                            as TextREReplace
-- import qualified Text.RE.TDFA.String                                                        as TextRETDFAString
-- import qualified Wallet.Emulator.Wallet                                                     as WalletEmulator
--------------------------------------------------------------------------------
import qualified MarketNFTV2.OffChain    as OffChain
import qualified MarketNFTV2.HelpersOffChain    as HelpersOffChain
import qualified MarketNFTV2.PABHelpers    as PABHelpers
import qualified MarketNFTV2.Helpers    as Helpers
import qualified Utils
import qualified MarketNFTV2.Deploy as Deploy
import qualified MarketNFTV2.Types as T
--------------------------------------------------------------------------------
runPABSimulator :: P.IO ()
runPABSimulator = Monad.void $ PABSimulator.runSimulationWith PABHelpers.handlers simulateInteractive



 
------------------------------------------------------------------------------

simulateInteractive  :: MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
simulateInteractive = do
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdownPAB <- PABServer.startServerDebug
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "********* PAB Server is running *********"
    mainLoopPAB (Just 1, 2) Nothing shutdownPAB

------------------------------------------------------------------------------

mainLoopPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB = do

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "MARKET NFT"

    case walletNro' of
        Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "1 - Choose Wallet"
        Just walletNro ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "1 - Choose Wallet (" ++ P.show walletNro ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "21 - New Protocol"

    case pABParams' of
        Nothing ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "22 - Choose Protocol"
        Just pABParams ->
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "22 - Choose Protocol (" ++ P.show (OffChain.ppProtocolID_CS pABParams ) ++ ")"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Protocol Actions"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "31 - Prepare Protocol"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "32 - Update Protocol"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "41 - Sell NFT"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "42 - Buy NFT"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "43 - Get Back NFT"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Others Actions"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "6 - Mint Tokens"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "7 - Split UtxO at Wallet"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "----"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "81 - All Balances"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "82 - UtxOs at Wallet"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "83 - UtxOs at Script"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "-----------"
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "99 - Exit"

    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Please enter an option:"
    opcion <- MonadIOClass.liftIO P.getLine

    case TextRead.readMaybe opcion :: Maybe Integer of
        Just 1 ->
            elegirWalletPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 21 ->
            createParamsPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 22 ->
            elegirParamsPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 31 ->
            protocolPreparePAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 32 ->
            protocolUpdatePAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 41 ->
            sellNFTPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 42 ->
            buyNFTPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 43 ->
            getBackNFTPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 6 ->
            mintTokensPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 7 ->
            splitUtxOPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 81 -> do
            let validatorHash = P.maybe Nothing (Just . OffChain.ppValidatorHash) pABParams'
            PABHelpers.balances (walletNro', walletCount) validatorHash shutdownPAB
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 82 ->
            uTxOAtWalletPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 83 ->
            uTxOAtScriptPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just 99 -> do
            let validatorHash = P.maybe Nothing (Just . OffChain.ppValidatorHash) pABParams'
            PABHelpers.balances (walletNro', walletCount) validatorHash shutdownPAB
            shutdownPAB
        _ -> mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB

-----------------------------------------------------------------------------------------

elegirWalletPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
elegirWalletPAB (_, walletCount') pABParams' shutdownPAB = do
    (walletNro, walletCount) <- PABHelpers.elegirWallet walletCount' True
    mainLoopPAB (Just walletNro, walletCount) pABParams' shutdownPAB

--------------------------------------------------------------------------------

uTxOAtWalletPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
uTxOAtWalletPAB (walletNro', walletCount) pABParams' shutdownPAB =
    case walletNro' of
        Just walletNro -> do

            blockchain <- PABSimulator.blockchain

            let

                uTxOutRefAt = fst <$>  PABHelpers.getUTxOsListInPABSimulator blockchain (PABHelpers.walletPaymentPubKeyHashAddress walletNro)

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "UtxOs at Wallet"

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts)) ["Values at: " ++ P.show uTxO ++ " " ++  P.show (LedgerBlockchain.value blockchain uTxO) | uTxO <- uTxOutRefAt ]

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) pABParams' shutdownPAB

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB

--------------------------------------------------------------------------------

uTxOAtScriptPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
uTxOAtScriptPAB (walletNro', walletCount) pABParams' shutdownPAB =

    case (walletNro',  pABParams') of
        (Just walletNro, Just pABParams) -> do

            let
                pABBalanceAtScriptParams = OffChain.BalanceAtScript OffChain.PABBalanceAtScriptParams{
                        OffChain.pbParams = pABParams
                    }

            cBalanceAtScript <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) pABBalanceAtScriptParams

            _ <- PABSimulator.waitUntilFinished cBalanceAtScript

            blockchain <- PABSimulator.blockchain

            let
                addressProtocol = OffChain.ppValidatorProtocolAddress pABParams
                address = OffChain.ppValidatorAddress pABParams

                uTxOutsProtocol = PABHelpers.getUTxOsListInPABSimulator blockchain addressProtocol
                uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address

                datumFrom :: LedgerApiV2.FromData d => (Ledger.TxOutRef, Ledger.TxOut) -> Maybe d
                datumFrom (uTxORef', uTxOut') =
                    let
                        txOutTx = Ledger.TxOutTx {
                            Ledger.txOutTxTx =  Ledger._emulatorTx $ Ledger.unOnChain $ Helpers.fromJust $ Ledger.transaction blockchain (Ledger.txOutRefId  uTxORef'),
                            Ledger.txOutTxOut = uTxOut'
                        }
                    in
                        case LedgerTx.txOutTxDatum txOutTx of
                            Nothing -> Nothing
                            Just d -> LedgerApiV2.fromBuiltinData  $ LedgerApiV2.getDatum d


                formatValues uTxORef = [P.show val   |  val <- LedgerValue.flattenValue $ Helpers.fromJust $ LedgerBlockchain.value blockchain uTxORef ]

                formatUTxOValuesProtocol :: [(Ledger.TxOutRef, Ledger.TxOut)] -> [P.String]
                formatUTxOValuesProtocol utxos  = concat [
                        "-----":
                        P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (uTxORef, uTxOut) utxos)): 
                        ("At: " ++ P.show uTxORef):
                        ("Datum: " ++  P.show (datumFrom @T.DatumProtocol (uTxORef, uTxOut))):
                        "Values: ": 
                        formatValues uTxORef | (uTxORef, uTxOut) <- utxos 
                    ]

                formatUTxOValues :: [(Ledger.TxOutRef, Ledger.TxOut)] -> [P.String]
                formatUTxOValues utxos  = concat [
                        "-----":
                        P.show ( 1 P.+  Helpers.fromJust(DataList.elemIndex (uTxORef, uTxOut) utxos)): 
                        ("At: " ++ P.show uTxORef):
                        ("Datum: " ++  P.show (datumFrom @T.DatumMarket (uTxORef, uTxOut))):
                        "Values: " :
                        formatValues uTxORef | (uTxORef, uTxOut) <- utxos 
                    ]
                

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "----------------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "UtxOs at Protocol: "++ P.show addressProtocol

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts)) $ formatUTxOValuesProtocol  uTxOutsProtocol

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "----------------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "UtxOs at Script: "++ P.show address

            mapM_ (PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts)) $ formatUTxOValues  uTxOuts

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "----------------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."

            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (walletNro', walletCount) (Just pABParams) shutdownPAB

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB

--------------------------------------------------------------------------------

splitUtxOPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
splitUtxOPAB (walletNro', walletCount) pABParams' shutdownPAB =

    case walletNro' of
        Just walletNro -> do
            let
                !ada_AC = LedgerValue.AssetClass (LedgerValue.adaSymbol, LedgerValue.adaToken)
                !minAda = HelpersOffChain.calculateMinAda 1 0 1 True

            !splitAmount <- PABHelpers.getAmount "ADA (lovelace)" ada_AC minAda
            let
                pABSplitUtxOParams = OffChain.SplitUtxO OffChain.PABSplitUtxOParams{
                        OffChain.psuSplitAmount = splitAmount
                    }

            cSplitUtxO <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) pABSplitUtxOParams

            _ <- PABSimulator.waitUntilFinished cSplitUtxO

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB

------------------------------------------------------------------------------

mintTokensPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
mintTokensPAB (walletNro', walletCount) pABParams' shutdownPAB =

    case walletNro' of

        Just walletNro -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "Enter policy number: "
            policyNum <- PABHelpers.getInt

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "Enter TokenName: "
            mintTokenNameBaseStr <- PABHelpers.getStr
            let mintTokenNameBaseBBS = Utils.stringToBuiltinByteString mintTokenNameBaseStr

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "Enter if you want to mint different TokenNames using the given TokenName as a base (y/n): "
            mintDiffTokenName <- PABHelpers.getBool

            pABMintFreeParams <-
                    if mintDiffTokenName then do
                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "Enter number of different TokenNames: "
                        pmmfMintDiifTokenNameCount <- PABHelpers.getInt

                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "Enter amount to mint for each TokenName: "
                        mintAmount <- PABHelpers.getInt

                        return $ OffChain.MintFree OffChain.PABMintFreeParams{
                                OffChain.pmmfMintPolicyNum = policyNum,
                                OffChain.pmmfMintTokenNameBase = mintTokenNameBaseBBS,
                                OffChain.pmmfMintDiifTokenNameCount = pmmfMintDiifTokenNameCount,
                                OffChain.pmmfMintAmount = mintAmount
                            }
                    else do
                        PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "Enter amount to mint: "
                        mintAmount <- PABHelpers.getInt

                        return $ OffChain.MintFree OffChain.PABMintFreeParams{
                                OffChain.pmmfMintPolicyNum = policyNum,
                                OffChain.pmmfMintTokenNameBase = mintTokenNameBaseBBS,
                                OffChain.pmmfMintDiifTokenNameCount = 0,
                                OffChain.pmmfMintAmount = mintAmount
                            }

            cMintFree <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) pABMintFreeParams

            _ <- PABSimulator.waitUntilFinished cMintFree

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "format time: " ++  PABHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) pABParams' shutdownPAB

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) Nothing shutdownPAB

------------------------------------------------------------------------------

createParamsPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
createParamsPAB (walletNro', walletCount) pABParams' shutdownPAB =

    case walletNro' of
        Just walletNro -> do

            let basePathFiles = "export/PABSimulator"

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "------"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Enter Protocol Name:"
            nombreProtocol <- PABHelpers.getStr

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Enter Admins:"

            mastersNros <- PABHelpers.elegirWallets walletCount []
            let
                masters = [
                    Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash adminsNro
                    | adminsNro <- mastersNros]

            blockchain <- PABSimulator.blockchain
            let
                uTxOutRefAt = fst <$> PABHelpers.getUTxOsListInPABSimulator blockchain (PABHelpers.walletPaymentPubKeyHashAddress walletNro)
                protocolID_TxOutRef = head uTxOutRefAt

            pABParams <- MonadIOClass.liftIO $ Deploy.exportarScripts basePathFiles nombreProtocol masters protocolID_TxOutRef 

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) (Just pABParams) shutdownPAB

        _ -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (walletNro', walletCount) pABParams' shutdownPAB

--------------------------------------------------------------------------------

elegirParamsPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
elegirParamsPAB (walletNro', walletCount) pABParams' shutdownPAB = do

    let basePathFiles = "export/PABSimulator"
    files <- MonadIOClass.liftIO $ SystemDirectory.listDirectory basePathFiles
    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Staking Pools:"
    let
        -- filterFiles = filter (
        --                     \n -> case DataTextSearch.indices "pABParams" (DataString.fromString n) of
        --                             (_:_) -> True
        --                             [] -> False
        --                     ) files
        !filterFiles = files
        enumerate x = zip [0..] x
        formatList list = concat
            [
                [P.show (n+1 :: Integer) ++ ": " ++ P.show item]
                |
                (n, item) <- enumerate list

            ]

    mapM_  ( PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts)) (formatList filterFiles)
    nombreProtocol <- PABHelpers.getFile basePathFiles filterFiles
    jsonFile <- MonadIOClass.liftIO $ Utils.readFile (basePathFiles SystemFilePathPosix.</> nombreProtocol SystemFilePathPosix.</> "PABParams-HEX.json")
    case DataAeson.decode jsonFile :: Maybe OffChain.PABParams  of
        Nothing -> elegirParamsPAB (walletNro', walletCount) pABParams' shutdownPAB
        Just pABParams -> mainLoopPAB (walletNro', walletCount) (Just pABParams) shutdownPAB

--------------------------------------------------------------------------------

protocolPreparePAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
protocolPreparePAB (walletNro', walletCount) pABParams' shutdownPAB =

    case (walletNro',  pABParams') of
        (Just walletNro, Just pABParams) -> do

            MonadIOClass.liftIO $ P.putStrLn "Choose Wallet for Protocol"
            (protocolNro, _) <- PABHelpers.elegirWallet walletCount False

            MonadIOClass.liftIO $ P.putStrLn "Base Points for Protocol"
            protocolPct <- PABHelpers.getInt

            MonadIOClass.liftIO $ P.putStrLn "Choose Wallet for Aldea"
            (aldeaNro, _) <- PABHelpers.elegirWallet walletCount False

            MonadIOClass.liftIO $ P.putStrLn "Base Points for Aldea"
            aldeaPct <- PABHelpers.getInt


            let
                protocolPkh = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash protocolNro
                aldeaPkh = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash aldeaNro

                contractDef = OffChain.PrepareProtocol OffChain.PABPrepareProtocolParams{
                    OffChain.ppParams = pABParams,
                    OffChain.ppAldeaPkh  = aldeaPkh,
                    OffChain.ppAldeaPct = aldeaPct,
                    OffChain.ppProtocolPkh = protocolPkh,
                    OffChain.ppProtocolPct= protocolPct
                }

            contract <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contractDef

            _ <- PABSimulator.waitUntilFinished contract

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "format time: " ++  PABHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) (Just pABParams) shutdownPAB

        (_, Just pABParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) (Just pABParams) shutdownPAB

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) Nothing shutdownPAB

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) Nothing shutdownPAB

--------------------------------------------------------------------------------

protocolUpdatePAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
protocolUpdatePAB (walletNro', walletCount) pABParams' shutdownPAB =
    case (walletNro',  pABParams') of
        (Just walletNro, Just pABParams) -> do

            MonadIOClass.liftIO $ P.putStrLn "Choose Wallet for Protocol"
            (protocolNro, _) <- PABHelpers.elegirWallet walletCount False

            MonadIOClass.liftIO $ P.putStrLn "Base Points for Protocol"
            protocolPct <- PABHelpers.getInt

            MonadIOClass.liftIO $ P.putStrLn "Choose Wallet for Aldea"
            (aldeaNro, _) <- PABHelpers.elegirWallet walletCount False

            MonadIOClass.liftIO $ P.putStrLn "Base Points for Aldea"
            aldeaPct <- PABHelpers.getInt

            let
                protocolPkh = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash protocolNro
                aldeaPkh = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash aldeaNro

                contractDef = OffChain.UpdateProtocol OffChain.PABUpdateProtocolParams{
                    OffChain.upParams = pABParams,
                    OffChain.upAldeaPkh  = aldeaPkh,
                    OffChain.upAldeaPct = aldeaPct,
                    OffChain.upProtocolPkh = protocolPkh,
                    OffChain.upProtocolPct= protocolPct
                }

            contract <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contractDef

            _ <- PABSimulator.waitUntilFinished contract

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "format time: " ++  PABHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoopPAB (Just walletNro, walletCount) (Just pABParams) shutdownPAB

        (_, Just pABParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) (Just pABParams) shutdownPAB

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) Nothing shutdownPAB

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) Nothing shutdownPAB

--------------------------------------------------------------------------------

sellNFTPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
sellNFTPAB (walletNro', walletCount) pABParams' shutdownPAB =
    case (walletNro',  pABParams') of
        (Just walletNro, Just pABParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = PABHelpers.walletAddress walletNro
                uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address

            MonadIOClass.liftIO $ P.putStrLn "Choose NFT to Sell"
            nft' <- PABHelpers.selectNFT uTxOuts blockchain

            case nft' of
                Nothing -> do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoopPAB  (Just walletNro, walletCount) (Just pABParams) shutdownPAB
                Just nft -> do
                    MonadIOClass.liftIO $ P.putStrLn "Choose ADA Price:"
                    priceADA <- PABHelpers.getMaybeInt

                    MonadIOClass.liftIO $ P.putStrLn "Choose ALDEA Price:"
                    priceALDEA <- PABHelpers.getMaybeInt

                    let

                        contractDef = OffChain.SellNFT OffChain.PABSellNFTParams{
                            OffChain.spParams = pABParams,
                            OffChain.spNFT  = nft,
                            OffChain.spPriceADA = priceADA,
                            OffChain.spPriceALDEA = priceALDEA
                        }

                    contract <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contractDef

                    _ <- PABSimulator.waitUntilFinished contract

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "format time: " ++  PABHelpers.getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoopPAB (Just walletNro, walletCount) (Just pABParams) shutdownPAB

        (_, Just pABParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) (Just pABParams) shutdownPAB

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) Nothing shutdownPAB

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) Nothing shutdownPAB

--------------------------------------------------------------------------------

buyNFTPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
buyNFTPAB (walletNro', walletCount) pABParams' shutdownPAB =
    case (walletNro',  pABParams') of
        (Just walletNro, Just pABParams) -> do

            blockchain <- PABSimulator.blockchain

            let
                address = OffChain.ppValidatorAddress pABParams
                uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address

            MonadIOClass.liftIO $ P.putStrLn "Choose NFT to Buy"
            nfts' <- PABHelpers.selectNFTs [] uTxOuts blockchain

            case nfts' of
                [] -> do
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoopPAB  (Just walletNro, walletCount) (Just pABParams) shutdownPAB
                _ -> do

                    let

                        contractDef = OffChain.BuyNFT OffChain.PABBuyNFTParams{
                            OffChain.bpParams = pABParams,
                            OffChain.bpNFTsBuy  = [T.NFTBuy {
                                    T.nNFT = nft,
                                    T.nSwPayADA = swPayADAInt
                                } | (_, swPayADAInt, nft) <- nfts']
                        }

                    contract <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contractDef

                    _ <- PABSimulator.waitUntilFinished contract

                    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
                    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) ""
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "slot: " ++  P.show slot
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "time: " ++  P.show posixTime
                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "format time: " ++  PABHelpers.getFormatTime posixTime

                    PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
                    Monad.void $ MonadIOClass.liftIO P.getLine
                    mainLoopPAB (Just walletNro, walletCount) (Just pABParams) shutdownPAB
        (_, Just pABParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) (Just pABParams) shutdownPAB

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) Nothing shutdownPAB

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) Nothing shutdownPAB

--------------------------------------------------------------------------------

getBackNFTPAB :: (Maybe Integer, Integer) -> Maybe OffChain.PABParams -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts))) ()
getBackNFTPAB (walletNro', walletCount) pABParams' shutdownPAB =
    case (walletNro',  pABParams') of
        (Just walletNro, Just pABParams) -> do

            MonadIOClass.liftIO $ P.putStrLn "Choose Wallet for Protocol"
            (protocolNro,_) <- PABHelpers.elegirWallet walletCount False
            MonadIOClass.liftIO $ P.putStrLn "Choose Wallet for Aldea"
            (aldeaNro,_) <- PABHelpers.elegirWallet walletCount False

            let
                protocolPkh = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash protocolNro
                aldeaPkh = Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash aldeaNro

                contractDef = OffChain.UpdateProtocol OffChain.PABUpdateProtocolParams{
                    OffChain.upParams = pABParams,
                    OffChain.upAldeaPkh  = aldeaPkh,
                    OffChain.upAldeaPct = 1,
                    OffChain.upProtocolPkh = protocolPkh,
                    OffChain.upProtocolPct= 1
                }

            contract <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contractDef

            _ <- PABSimulator.waitUntilFinished contract

            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) ""
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "slot: " ++  P.show slot
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "time: " ++  P.show posixTime
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) $ "format time: " ++  PABHelpers.getFormatTime posixTime

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine
            mainLoopPAB (Just walletNro, walletCount) (Just pABParams) shutdownPAB

        (_, Just pABParams) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) (Just pABParams) shutdownPAB

        (Just walletNro, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Just walletNro, walletCount) Nothing shutdownPAB

        (_, _) -> do

            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Choose Wallet and PAB Pool Params"
            PABSimulator.logString @(PABEffectsContractBuiltin.Builtin OffChain.ValidatorContracts) "Press return to continue..."
            Monad.void $ MonadIOClass.liftIO P.getLine

            mainLoopPAB (Nothing, walletCount) Nothing shutdownPAB

--------------------------------------------------------------------------------
