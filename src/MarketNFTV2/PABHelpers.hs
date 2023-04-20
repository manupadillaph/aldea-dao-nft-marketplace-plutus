-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveAnyClass             #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
-- {-# LANGUAGE NumericUnderscores         #-}
-- {-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

{- HLINT ignore "Use camelCase" -}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}
-----------------------------------------------------------------------------------------
module MarketNFTV2.PABHelpers where
-----------------------------------------------------------------------------------------
-- Import Externos
-----------------------------------------------------------------------------------------
import qualified Control.Concurrent.STM                                 as ConcurrentSTM (atomically)
import qualified Control.Monad.IO.Class                                 as MonadIOClass (MonadIO (..))
import qualified Control.Monad.Freer                                    as MonadFreer (interpret)
import qualified Control.Monad.Freer.Internal                           as MonadFreerInternal (Eff)
-- import qualified Cardano.Node.Emulator.TimeSlot                         as CardanoNodeEmulatorTimeSlot
import qualified Data.Default                                           as DataDefault (def)
import qualified Data.Fixed                                             as DataFixed (Pico, Fixed ( MkFixed ))
import qualified Data.List                                              as DataList
import qualified Data.Map                                               as DataMap
import qualified Data.Maybe                                             as DataMaybe
import qualified Data.ByteString                                                            as DataByteString
import qualified Data.Time.Clock                                        as DataTimeClock (secondsToNominalDiffTime)
import qualified Data.Time.Clock.POSIX                                  as DataTimeClockPOSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format                                       as DataTimeFormat (defaultTimeLocale, formatTime)
import qualified Data.Typeable                                             as DataTypeable
import qualified Ledger
-- import qualified Ledger.Ada                                             as LedgerAda
import qualified Ledger.Address                                         as LedgerAddress (Address)
import qualified Ledger.Blockchain                                      as LedgerBlockchain
import qualified Ledger.CardanoWallet                                   as LedgerCardanoWallet
import qualified Ledger.TimeSlot                                        as LedgerTimeSlot
import qualified Ledger.Value                                           as LedgerValue
-- import qualified Playground.Contract                                 as PlaygroundContract (IO)
import qualified Prelude                                                as P
import qualified Plutus.PAB.Core                                        as PABCore (PABEffects)
import qualified Plutus.PAB.Effects.Contract.Builtin                    as PABEffectsContractBuiltin (Builtin, BuiltinHandler(contractHandler), handleBuiltin, HasDefinitions)
import qualified Plutus.PAB.Simulator                                   as PABSimulator
-- import qualified Plutus.V2.Ledger.Address                            as LedgerAddressV2
import qualified Plutus.V2.Ledger.Api                                   as LedgerApiV2
-- import qualified Plutus.V2.Ledger.Value                              as LedgerValueV2
-- import qualified Plutus.V2.Ledger.Tx                                 as LedgerTxV2 (txOutDatum)
-- import qualified PlutusTx
import qualified PlutusTx.Builtins.Class                                                    as TxBuiltinsClass
import qualified PlutusTx.Builtins.Internal                             as TxBuiltinsInternal hiding (head, consByteString)
import qualified PlutusTx.Eq                                            as PlutusTxEq
import           PlutusTx.Prelude                                       hiding (unless)
import qualified Prettyprinter.Internal                                 as PrettyprinterInternal
import qualified System.Directory                                       as SystemDirectory
import qualified System.FilePath.Posix                                  as SystemFilePathPosix
import qualified Text.Hex                                               as TextHex
import qualified Text.Read                                              as TextRead (readMaybe)
import qualified Wallet.Emulator.Wallet                                 as WalletEmulator
------------------------------------------------------------------------------------------
-- Import Internos
------------------------------------------------------------------------------------------
import qualified MarketNFTV2.HelpersOffChain                                   as HelpersOffChain
import qualified MarketNFTV2.Helpers                                   as HelpersOnChain
import qualified Utils
import qualified MarketNFTV2.Helpers as Helpers
import qualified Ledger.Ada as LedgerAda
------------------------------------------------------------------------------------------
-- Modulo
------------------------------------------------------------------------------------------

handlers :: (PrettyprinterInternal.Pretty a, PABEffectsContractBuiltin.HasDefinitions a) => PABSimulator.SimulatorEffectHandlers (PABEffectsContractBuiltin.Builtin a)
handlers = PABSimulator.mkSimulatorHandlers  DataDefault.def P.$ MonadFreer.interpret (PABEffectsContractBuiltin.contractHandler PABEffectsContractBuiltin.handleBuiltin)

------------------------------------------------------------------------------------------

getWallet :: Integer -> WalletEmulator.Wallet
getWallet = WalletEmulator.knownWallet

------------------------------------------------------------------------------------------

walletPaymentPubKeyHash :: Integer -> Ledger.PaymentPubKeyHash
walletPaymentPubKeyHash walletNumber = LedgerCardanoWallet.paymentPubKeyHash (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletAddress :: Integer -> Ledger.Address
walletAddress walletNumber = LedgerCardanoWallet.mockWalletAddress (LedgerCardanoWallet.fromWalletNumber $ LedgerCardanoWallet.WalletNumber walletNumber)

walletPaymentPubKeyHashAddress :: Integer -> LedgerAddress.Address
walletPaymentPubKeyHashAddress walletNumber = Ledger.pubKeyHashAddress (walletPaymentPubKeyHash walletNumber) Nothing

------------------------------------------------------------------------------------------

getUTxOsListInPABSimulator :: Ledger.Blockchain -> LedgerAddress.Address -> [(Ledger.TxOutRef, Ledger.TxOut)]
getUTxOsListInPABSimulator blockchain addr = do
    let
        !unspentOutputList = Ledger.unspentOutputs blockchain
        !uTxOs = [(txOutRef, txOut)  | (txOutRef, txOut) <- DataMap.toList unspentOutputList, Utils.cardanoAddressToAddress (Ledger.txOutAddress txOut) == addr]
    uTxOs

------------------------------------------------------------------------------------------

getFormatTime :: LedgerApiV2.POSIXTime -> P.String
getFormatTime posixTime =
    let
        milisegundosFixedPico :: DataFixed.Pico
        !milisegundosFixedPico = DataFixed.MkFixed  (LedgerApiV2.getPOSIXTime posixTime * 1000000000)
        !seconds = DataTimeClock.secondsToNominalDiffTime milisegundosFixedPico
    in
        DataTimeFormat.formatTime DataTimeFormat.defaultTimeLocale  "%c" $ DataTimeClockPOSIX.posixSecondsToUTCTime seconds

------------------------------------------------------------------------------------------

getAmountWithMax :: forall a. (DataTypeable.Typeable a ,PrettyprinterInternal.Pretty a, PABEffectsContractBuiltin.HasDefinitions a) => P.String -> Ledger.AssetClass -> Integer -> Integer -> MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  Integer
getAmountWithMax unit_UI unit_AC minAmount maxAmount = do
    let

        unit_Str = unit_UI
            -- TODO: mostrar el hex bien
            -- ++ " ("
            -- if LedgerApiV2.adaSymbol /= fst (LedgerValue.unAssetClass unit_AC) then
            --     let 
            --         --  $ Utils.stringToStrictText
            --         cs = P.show (  LedgerValue.unCurrencySymbol $ fst $ LedgerValue.unAssetClass unit_AC) 
            --     in
            --         cs ++ "." ++ P.show (LedgerValue.unTokenName $ snd $ LedgerValue.unAssetClass unit_AC)
            -- else 
            --     ""
            -- ++ ")"

    MonadIOClass.liftIO $ P.putStrLn $ "Enter amount of " ++ unit_Str ++ " (min: " ++ P.show minAmount ++ " - max: " ++ P.show maxAmount ++ "): "

    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= minAmount && x <= maxAmount then return x
            else do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input"
                getAmountWithMax unit_UI unit_AC minAmount maxAmount
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input"
            getAmountWithMax unit_UI unit_AC minAmount maxAmount

------------------------------------------------------------------------------------------

getAmount :: P.String -> Ledger.AssetClass -> Integer -> MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  Integer
getAmount unit_UI unit_AC minAmount  = do
    let

        unit_Str = unit_UI
            -- TODO: mostrar el hex bien
            -- ++ " ("
            -- if LedgerApiV2.adaSymbol /= fst (LedgerValue.unAssetClass unit_AC) then
            --     let 
            --         --  $ Utils.stringToStrictText
            --         cs = P.show (  LedgerValue.unCurrencySymbol $ fst $ LedgerValue.unAssetClass unit_AC) 
            --     in
            --         cs ++ "." ++ P.show (LedgerValue.unTokenName $ snd $ LedgerValue.unAssetClass unit_AC)
            -- else 
            --     ""
            -- ++ ")"

    MonadIOClass.liftIO $ P.putStrLn $ "Enter amount of " ++ unit_Str ++ " (min: " ++ P.show minAmount ++ "): "
    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= minAmount then return x
            else do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input"
                getAmount unit_UI unit_AC minAmount
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input"
            getAmount unit_UI unit_AC minAmount

-----------------------------------------------------------------------------------------

getInt :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  Integer
getInt = do
    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= 0 then
                return x
            else do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                getInt
        Nothing -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
            getInt
-----------------------------------------------------------------------------------------

getMaybeInt :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  (Maybe Integer)
getMaybeInt = do
    !numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt of
        Just x ->
            if x >= 0 then
                return (Just x)
            else do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                getMaybeInt
        Nothing -> do
            if length numberSrt == 0 then return Nothing
            else do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                getMaybeInt

-----------------------------------------------------------------------------------------

getStr :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  P.String
getStr = do
    !srt <- MonadIOClass.liftIO P.getLine
    if length srt == 0
    then do
        MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
        getStr
    else return srt

-----------------------------------------------------------------------------------------

getBool :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                 Bool
getBool = do
    !srt <- MonadIOClass.liftIO P.getLine
    if length srt == 0
    then getBool
    else
        case srt of
            "y" -> return True
            "n" -> return False
            _ -> do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                getBool

-----------------------------------------------------------------------------------------

getTime :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) LedgerApiV2.POSIXTime
getTime defTime lowerLimit = do
    str <- MonadIOClass.liftIO P.getLine
    if length str == 0
    then do
        return defTime
    else
        case TextRead.readMaybe str :: Maybe Integer of
            Just x ->
                if LedgerApiV2.POSIXTime x >= lowerLimit then do
                    return $ LedgerApiV2.POSIXTime x
                else do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again:"
                    getTime defTime lowerLimit
            _ -> do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again:"
                getTime defTime lowerLimit

-----------------------------------------------------------------------------------------

getUnitName :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  P.String
getUnitName = do
    MonadIOClass.liftIO $ P.putStrLn "Leave empty to use ADA (lovelace)"
    str <- MonadIOClass.liftIO P.getLine
    if length str == 0
    then return "ADA (loveLace)"
    else return str

-----------------------------------------------------------------------------------------

getCurrencySymbol :: MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  LedgerApiV2.CurrencySymbol
getCurrencySymbol = do
    MonadIOClass.liftIO $ P.putStrLn "Leave empty to use ADA (lovelace)"
    cS_Str <- MonadIOClass.liftIO P.getLine
    if length cS_Str P.== 0 || length cS_Str P.== 56 then do
        let
            isHexOk hex' =
                case hex' of
                    Nothing -> False
                    _       -> True

            hex = TextHex.decodeHex $ Utils.stringToStrictText cS_Str
        if isHexOk hex then
            let hex_Str = DataMaybe.fromJust hex
            in  if DataByteString.length hex_Str P.== 0
                    then do
                        return LedgerApiV2.adaSymbol
                    else do
                        return $ LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin hex_Str
        else do
            MonadIOClass.liftIO $ P.putStrLn "Invalid Currency Symbol, try again:"
            getCurrencySymbol
    else do
        MonadIOClass.liftIO $ P.putStrLn "Invalid Currency Symbol, try again:"
        getCurrencySymbol

-----------------------------------------------------------------------------------------

getTokenName :: Bool -> MonadFreerInternal.Eff
                  (PABCore.PABEffects
                     (PABEffectsContractBuiltin.Builtin a)
                     (PABSimulator.SimulatorState
                        (PABEffectsContractBuiltin.Builtin a)))
                  LedgerApiV2.TokenName
getTokenName canBeEmpty = do
    if canBeEmpty then do
        MonadIOClass.liftIO $ P.putStrLn "Enter TokenName (Leave empty to use ADA (lovelace)):"
        tN_Str <- MonadIOClass.liftIO P.getLine
        if length tN_Str == 0
        then
            return LedgerApiV2.adaToken
        else
            return $ LedgerApiV2.TokenName $ Utils.stringToBuiltinByteString tN_Str
    else do
        MonadIOClass.liftIO $ P.putStrLn "Enter TokenName (can't be empty, must use a TokenName):"
        tN_Str <- MonadIOClass.liftIO P.getLine
        if length tN_Str == 0
        then do
            getTokenName canBeEmpty
        else do
            return $ LedgerApiV2.TokenName $ Utils.stringToBuiltinByteString tN_Str

-----------------------------------------------------------------------------------------

getFile :: P.String -> [P.String] -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) P.String
getFile path list = do

    MonadIOClass.liftIO $ P.putStrLn "Enter File number:"
    !numeroStr <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numeroStr of
        Just n -> do
            if n <= length list && n > 0 then do
                let !nombre = list!!(n-1)
                MonadIOClass.liftIO $ P.putStrLn $ "File: " ++ nombre
                !exist <- MonadIOClass.liftIO $ SystemDirectory.doesFileExist (path SystemFilePathPosix.</> nombre SystemFilePathPosix.</> "PABParams-HEX.json")
                if exist then do
                    MonadIOClass.liftIO $ P.putStrLn "ok"
                    return nombre
                else do
                    MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                    getFile path list
            else do
                MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
                getFile path list
        Nothing -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input, try again"
            getFile path list

-----------------------------------------------------------------------------------------

isEqWallet :: WalletEmulator.Wallet -> WalletEmulator.Wallet -> Bool
isEqWallet w w' =
    TxBuiltinsInternal.BuiltinString (WalletEmulator.toBase16 $ WalletEmulator.getWalletId w) PlutusTxEq.== TxBuiltinsInternal.BuiltinString(WalletEmulator.toBase16 $ WalletEmulator.getWalletId w')

-----------------------------------------------------------------------------------------

fromWallet :: Integer -> WalletEmulator.Entity -> Bool
fromWallet numWallet entity =
    case entity of
        WalletEmulator.WalletEntity wallet  -> isEqWallet wallet (getWallet numWallet)
        _                                   -> False

-----------------------------------------------------------------------------------------

fromScript :: LedgerApiV2.ValidatorHash -> WalletEmulator.Entity -> Bool
fromScript hash entity =
    case entity of
        WalletEmulator.ScriptEntity scriptHash ->
            hash == scriptHash
        _ -> False

-----------------------------------------------------------------------------------------

walletFromEntity :: WalletEmulator.Entity -> Maybe WalletEmulator.Wallet
walletFromEntity entity =
    case entity of
        WalletEmulator.WalletEntity wallet -> Just wallet
        _ -> Nothing

-----------------------------------------------------------------------------------------

balances :: (Maybe Integer, Integer) -> Maybe LedgerApiV2.ValidatorHash -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) () -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) ()
balances (_, walletCount) validatorHash' _ = do

    MonadIOClass.liftIO $ P.putStrLn "Balances:"

    !balances' <- PABSimulator.currentBalances

    -- MonadIOClass.liftIO $ P.putStrLn ("Balances:" ++ P.show balances')

    let
        -- (entity, value) <- DataMap.toList balances'
        !balanceList = DataMap.toList balances'
        formatWallets = concat [
            let
                fromWalletEntity walletNro' (entity, _) = fromWallet walletNro' entity
                entiyValue' = find (fromWalletEntity walletNro ) balanceList
            in
                case entiyValue' of
                    Nothing -> []
                    Just (_, value) ->
                        [
                            "----------------" ,
                            "#: " ++ P.show walletNro,
                            "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro) ,
                            "Value: " ++ P.show value
                        ] | walletNro <- [1..walletCount]
            ]

    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatWallets

    case validatorHash' of
        Just validatorHash ->
            mapM_ (MonadIOClass.liftIO . P.putStrLn)
                ["Script: " ++ P.show validatorHash ++ " " ++  P.show value | (entity, value) <- DataMap.toList balances', fromScript validatorHash entity ]

        _ ->
            MonadIOClass.liftIO $ P.putStrLn ""

    slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
    let posixTime = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot

    MonadIOClass.liftIO $ P.putStrLn ""
    MonadIOClass.liftIO $ P.putStrLn $ "slot: " ++  P.show slot
    MonadIOClass.liftIO $ P.putStrLn $ "time: " ++  P.show posixTime
    MonadIOClass.liftIO $ P.putStrLn $ "format time: " ++  getFormatTime posixTime

----------------------------------------------------------------------------------------

selectUTxO :: LedgerValue.AssetClass -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a)))  (Maybe (Integer, Ledger.TxOutRef))
selectUTxO unit_AC uTxOuts blockchain = do
    let
        !uTxOutsWithAC' =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) unit_AC > 0 ]

    case uTxOutsWithAC' of
        [] -> do
            MonadIOClass.liftIO $ P.putStrLn "There is no UtxO to choose"
            return Nothing
        uTxOutsWithAC ->  do
            let
                datumFrom _ =
                    "TODO: get Datum"

                formatValues uTxORef = [P.show val   |  val <- LedgerValue.flattenValue $ HelpersOnChain.fromJust $ LedgerBlockchain.value blockchain uTxORef ]

                formatUTxOValues = concat [
                    "----------------" :
                    ("#: " ++ P.show ( 1 P.+  HelpersOnChain.fromJust(DataList.elemIndex (uTxORef, uTxOut) uTxOutsWithAC))) :
                    ("At: " ++ P.show uTxORef) :
                    ("Datum: " ++  datumFrom uTxOut) : formatValues uTxORef | (uTxORef, uTxOut) <- uTxOutsWithAC
                    ]

            MonadIOClass.liftIO $ P.putStrLn "----------------"
            MonadIOClass.liftIO $ P.putStrLn "Choose UtxO:"

            mapM_ (MonadIOClass.liftIO . P.putStrLn) formatUTxOValues

            MonadIOClass.liftIO $ P.putStrLn "----------------"

            !opcionUTxO <- MonadIOClass.liftIO P.getLine

            case TextRead.readMaybe opcionUTxO :: Maybe Integer of
                Just x -> do
                    if x >= 1 && x <= length uTxOutsWithAC then do
                        let
                            !new = (x, fst $ uTxOutsWithAC!!(x-1))
                        return (Just new)
                    else
                        selectUTxO unit_AC uTxOuts blockchain
                _ ->
                    selectUTxO unit_AC uTxOuts blockchain

------------------------------------------------------------------------------------------

selectUTxOs :: LedgerValue.AssetClass -> [(Integer, Ledger.TxOutRef)] -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a)))  [(Integer, Ledger.TxOutRef)]
selectUTxOs unit_AC opciones uTxOuts blockchain = do
    let

        !uTxOutsWithAC =
            [ (txOutRef, txOut) | (txOutRef, txOut) <- uTxOuts, LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) unit_AC > 0 ]

        datumFrom _ =
            "TODO: get Datum"

        formatValues uTxORef = [P.show val   |  val <- LedgerValue.flattenValue $ HelpersOnChain.fromJust $ LedgerBlockchain.value blockchain uTxORef ]

        formatUTxOValues = concat [
            "----------------" :
            ("#: " ++ P.show ( 1 P.+  HelpersOnChain.fromJust(DataList.elemIndex (uTxORef, uTxOut) uTxOutsWithAC))) :
            ("At: " ++ P.show uTxORef) : ("Datum: " ++  datumFrom uTxOut) :
            formatValues uTxORef
            | (uTxORef, uTxOut) <- uTxOutsWithAC ]

        formatSelected :: [(Integer, Ledger.TxOutRef)] -> [P.String]
        formatSelected opciones' = concat [  ["----------------", P.show numOpcion, P.show uTxORef] | (numOpcion, uTxORef) <- opciones' ]

    MonadIOClass.liftIO $ P.putStrLn "----------------"
    MonadIOClass.liftIO $ P.putStrLn "Choose UtxO:"

    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatUTxOValues

    MonadIOClass.liftIO $ P.putStrLn "----------------"
    MonadIOClass.liftIO $ P.putStrLn "Selected:"
    mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatSelected opciones)

    MonadIOClass.liftIO $ P.putStrLn "----------------"
    MonadIOClass.liftIO $ P.putStrLn "Option (0 to finish):"

    opcionUTxO <- MonadIOClass.liftIO P.getLine

    case TextRead.readMaybe opcionUTxO :: Maybe Integer of
        Just 0 ->
            return opciones
        Just x -> do

            if x >= 1 && x <= length uTxOutsWithAC then do
                let
                    new = (x, fst $ uTxOutsWithAC!!(x-1))
                    news = new : filter (new/=) opciones
                selectUTxOs unit_AC news uTxOuts blockchain
            else
                selectUTxOs unit_AC opciones uTxOuts blockchain
        _ ->
            selectUTxOs unit_AC opciones uTxOuts blockchain

------------------------------------------------------------------------------------------

elegirWallet :: Integer -> Bool -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) (Integer, Integer)
elegirWallet walletCount swAdd = do
    MonadIOClass.liftIO $ P.putStrLn "Wallets:"

    let
        formatWallets = concat [ [
                    "----------------" ,
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro)
                ] | walletNro <- [1..walletCount]
            ]
    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatWallets

    if swAdd then do
        MonadIOClass.liftIO $ P.putStrLn "----------------"
        MonadIOClass.liftIO $ P.putStrLn "0 - Add Wallet (up to 10)"
        MonadIOClass.liftIO $ P.putStrLn "----------------"
    else
        return ()

    MonadIOClass.liftIO $ P.putStrLn "Choose Wallet:"
    numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt :: Maybe Integer of
        Just x ->
            if x == 0 && swAdd then do
                -- PABSimulator.addWalletWith (Just $ LedgerAda.lovelaceOf 100)  
                elegirWallet (walletCount+1) swAdd
            else
                if x >= 1 && x <= walletCount then 
                    return (x, walletCount)
                else
                    elegirWallet walletCount swAdd
        _ ->
            elegirWallet walletCount swAdd

-----------------------------------------------------------------------------------------

elegirWallets :: Integer -> [Integer] -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) [Integer]
elegirWallets walletCount opciones = do
    MonadIOClass.liftIO $ P.putStrLn "Wallets:"
    let
        formatWallets = concat [ [
                    "----------------" ,
                    "#: " ++ P.show walletNro,
                    "Pk: " ++ P.show (walletPaymentPubKeyHash walletNro)
                ] | walletNro <- [1..walletCount]
            ]

        formatSelected :: [Integer] -> [P.String]
        formatSelected opciones' =
            concat [  ["----------------", P.show walletNro] | walletNro <- opciones' ]

    mapM_ (MonadIOClass.liftIO . P.putStrLn) formatWallets
    MonadIOClass.liftIO $ P.putStrLn "----------------"
    MonadIOClass.liftIO $ P.putStrLn "Selected:"
    mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatSelected opciones)
    MonadIOClass.liftIO $ P.putStrLn "----------------"
    MonadIOClass.liftIO $ P.putStrLn "Option (0 to finish):"
    numberSrt <- MonadIOClass.liftIO P.getLine
    case TextRead.readMaybe numberSrt :: Maybe Integer of
        Just 0 ->
            return opciones
        Just x ->
            if x >= 1 && x <= walletCount then
                let
                    new = x
                    news = new : filter (new/=) opciones
                in elegirWallets walletCount news
             else
                elegirWallets walletCount opciones
        _ ->
            elegirWallets walletCount opciones

---------------------------------------------------------------------------------------

selectNFT ::[(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) (Maybe LedgerValue.AssetClass)
selectNFT uTxOuts blockchain = do

    -- return $ 
    let

        !addAllValues = foldl (<>) (LedgerAda.lovelaceValueOf 0) [ Ledger.txOutValue txOut | (_, txOut) <- uTxOuts ]

        !nfts' = [ LedgerValue.AssetClass (cs, tk) | (cs, tk, am) <- Helpers.flattenValue addAllValues, am == 1]

    case nfts' of
        [] -> do
            MonadIOClass.liftIO $ P.putStrLn "There is no NFTs to choose"
            return Nothing
        nfts ->  do
            let

                formatNFTS = concat [
                        ["----------------",
                        "#: " ++ P.show ( 1 P.+  HelpersOnChain.fromJust(DataList.elemIndex nft nfts)),
                        "NFT: " ++ P.show nft]
                        | nft <- nfts
                    ]

            MonadIOClass.liftIO $ P.putStrLn "----------------"
            MonadIOClass.liftIO $ P.putStrLn "Choose NFT:"

            mapM_ (MonadIOClass.liftIO . P.putStrLn) formatNFTS

            MonadIOClass.liftIO $ P.putStrLn "----------------"

            !opcionNFT <- MonadIOClass.liftIO P.getLine

            case TextRead.readMaybe opcionNFT :: Maybe Integer of
                Just x -> do
                    if x >= 1 && x <= length nfts then do
                        let !new = nfts!!(x-1)
                        return $ Just new
                    else
                        selectNFT uTxOuts blockchain
                _ ->
                    selectNFT uTxOuts blockchain

---------------------------------------------------------------------------------------

selectNFTs :: [(Integer, Integer, LedgerValue.AssetClass)] -> [(Ledger.TxOutRef, Ledger.TxOut)] -> LedgerBlockchain.Blockchain -> MonadFreerInternal.Eff (PABCore.PABEffects (PABEffectsContractBuiltin.Builtin a) (PABSimulator.SimulatorState (PABEffectsContractBuiltin.Builtin a))) [(Integer, Integer, LedgerValue.AssetClass)]
selectNFTs opciones uTxOuts blockchain = do

    -- return $ 
    let

        !addAllValues = foldl (<>) (LedgerAda.lovelaceValueOf 0) [ Ledger.txOutValue txOut | (_, txOut) <- uTxOuts ]

        !nfts' = [ LedgerValue.AssetClass (cs, tk) | (cs, tk, am) <- Helpers.flattenValue addAllValues, am == 1]

    case nfts' of
        [] -> do
            MonadIOClass.liftIO $ P.putStrLn "There is no NFTs to choose"
            return []
        nfts ->  do
            let

                formatNFTS = concat [
                        ["----------------",
                        "#: " ++ P.show ( 1 P.+  HelpersOnChain.fromJust(DataList.elemIndex nft nfts)),
                        "NFT: " ++ P.show nft]
                        | nft <- nfts
                    ]

                formatSelected :: [(Integer, Integer, LedgerValue.AssetClass)] -> [P.String]
                formatSelected opciones' = concat [  ["----------------", P.show numOpcion, P.show swPayWithADA, P.show nft] | (numOpcion, swPayWithADA, nft) <- opciones' ]


            MonadIOClass.liftIO $ P.putStrLn "----------------"
            MonadIOClass.liftIO $ P.putStrLn "Choose NFT:"

            mapM_ (MonadIOClass.liftIO . P.putStrLn) formatNFTS

            MonadIOClass.liftIO $ P.putStrLn "----------------"
            MonadIOClass.liftIO $ P.putStrLn "Selected:"
            mapM_ (MonadIOClass.liftIO . P.putStrLn) (formatSelected opciones)

            MonadIOClass.liftIO $ P.putStrLn "----------------"
            MonadIOClass.liftIO $ P.putStrLn "Option (0 to finish):"

            !opcionNFT <- MonadIOClass.liftIO P.getLine

            case TextRead.readMaybe opcionNFT :: Maybe Integer of
                Just 0 ->
                    return opciones
                Just x -> do
                    if x >= 1 && x <= length nfts then do
                        MonadIOClass.liftIO $ P.putStrLn "Pay with ADA? (y/n)"
                        swPayADABool <- getBool
                        let 
                            swPayADAInt  = 
                                if swPayADABool then
                                    1 :: Integer
                                else
                                    0 :: Integer
                                    
                            !new = (x, swPayADAInt, nfts!!(x-1))
                            !news = new : filter (\(i, _, _) -> i P./= x) opciones
                        selectNFTs news uTxOuts blockchain
                    else
                        selectNFTs opciones uTxOuts blockchain
                _ ->
                    selectNFTs opciones uTxOuts blockchain


