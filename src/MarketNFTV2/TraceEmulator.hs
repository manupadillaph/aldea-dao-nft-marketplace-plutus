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
{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------
module MarketNFTV2.TraceEmulator where
------------------------------------------------------------------------------------------
-- Import Externos
------------------------------------------------------------------------------------------
import qualified Control.Lens                               as ControlLens
import qualified Control.Monad.IO.Class                                                     as MonadIOClass (MonadIO (..))
import qualified Control.Monad.Freer.Extras          as MonadExtras
import qualified Data.ByteString.Short                         as DataByteStringShort
import qualified Data.ByteString.Lazy                          as DataByteStringLazy
import qualified Data.Default                        as DataDefault (def) 
import qualified Data.Map                            as DataMap
import qualified Data.Maybe                                    as DataMaybe
import qualified Codec.Serialise                               as CodecSerialise
import qualified Ledger.Ada                                    as LedgerAda
import qualified Ledger.Address                                as LedgerAddress
-- import qualified Ledger.TimeSlot                     as LedgerTimeSlot (slotToEndPOSIXTime)    
import qualified Ledger.Value                                  as LedgerValue
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Scripts                as UtilsScriptsV2
import qualified Plutus.V1.Ledger.ProtocolVersions             as LedgerProtocolVersionsV1
import qualified Plutus.V1.Ledger.Scripts                      as LedgerScriptsV1
import qualified Plutus.V2.Ledger.Api                          as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                     as LedgerContextsV2
import qualified Plutus.V2.Ledger.EvaluationContext            as LedgerEvaluationContextV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap                             as TxAssocMap
import           PlutusTx.Prelude
import qualified Prelude                                       as P
import qualified PlutusTx.Builtins                             as TxBuiltins
import qualified Prettyprinter  (defaultLayoutOptions, layoutPretty, pretty)
import qualified Prettyprinter.Render.String  as Prettyprinter  (renderString)
import qualified Prettyprinter.Render.Text  as Prettyprinter (renderStrict)
import qualified System.Directory                                                           as SystemDirectory
import qualified System.Environment                                                         as SystemEnvironment (lookupEnv)
import qualified System.FilePath.Posix                                                      as SystemFilePathPosix
import qualified System.IO.Unsafe as SystemIOUnsafe
import qualified System.IO as SystemIO

---------------------------------------------------

import qualified Wallet.Emulator.Wallet              as WalletEmulator
import qualified Wallet.Emulator.Types               as WalletEmulatorTypes
import qualified Wallet.Emulator.MultiAgent               as WalletEmulatorMultiAgent
import qualified Plutus.Contract                     as PlutusContract 
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Schema                              (ToSchema)
import qualified Data.Aeson                          as DataAeson 
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Text                           as DataText (Text)
import qualified Control.Monad                       as Monad (void)
import qualified Control.Monad.Freer.Internal                     as MonadFreerInternal  
import qualified Ledger.Constraints                                                                 as LedgerConstraints
import qualified Text.Printf                                                                        as TextPrintf (printf)
import qualified Data.Void                                                                          as DataVoid (Void)
import qualified Playground.Contract                                                                as PlaygroundContract (mkSchemaDefinitions)
import Data.Int (Int64)
import Data.Monoid (Sum)
import qualified Ledger 
import Plutus.Trace.Emulator (EmulatorConfig, EmulatorTrace)
import qualified Plutus.Trace.Emulator.Extract  as TraceEmulatorExtract
import qualified Plutus.Trace.Emulator                         as TraceEmulator
import qualified Plutus.Trace.Emulator.Types as TraceEmulatorTypes

--------------------------------------------------------------------------------
import qualified MarketNFTV2.Deploy as Deploy
import qualified Utils
import qualified MarketNFTV2.Types        as T 
import qualified MarketNFTV2.OffChain      as OffChain
import qualified MarketNFTV2.OnChain      as OnChain
import qualified MarketNFTV2.ProtocolIDPolicy as OnChain
import qualified MarketNFTV2.HelpersOffChain      as HelpersOffChain  
--------------------------------------------------------------------------------
-- runTraceEmulator :: P.IO ()
-- runTraceEmulator = TraceEmulator.runEmulatorTraceIO $ do

--     Monad.void $ TraceEmulator.waitNSlots 1

------------------------------------------------------------------------------------------

getUtxos :: LedgerAddress.Address -> TraceEmulator.EmulatorTrace [(Ledger.TxOutRef, Ledger.TxOut)]
getUtxos addr = do
    state <- TraceEmulator.chainState
    let utxoIndex = Ledger.getIndex $ state ControlLens.^. TraceEmulator.index 
        utxos =  [(oref, o) | (oref, o) <- DataMap.toList utxoIndex, Utils.cardanoAddressToAddress (Ledger.txOutAddress o) == addr]
    return utxos  

------------------------------------------------------------------------------------------

emulatorConfig :: TraceEmulator.EmulatorConfig
emulatorConfig = TraceEmulator.EmulatorConfig (Left $ DataMap.fromList [(WalletEmulator.knownWallet w, v) | w <- [1 .. 1]]) DataDefault.def 
  where
    v :: LedgerApiV2.Value
    v = LedgerAda.lovelaceValueOf 2000_000_000 

------------------------------------------------------------------------------------------

-- traceConfig :: TraceEmulator.TraceConfig
-- traceConfig = TraceEmulator.TraceConfig
--   { 
--     showEvent  = TraceEmulator.defaultShowEvent, -- ^ Function to decide how to print the particular events.
--     outputHandle = SystemIO.stdout -- withFile "/tmp/trace-log.txt" WriteMode $ \h -> h
--     -- ^ Where to print the outputs to. Default: 'System.IO.stdout'
--     -- traceConfigMinLogLevel = MonadExtras.Debug
--     -- Debug	 
--     -- Info	 
--     -- Notice	 
--     -- Warning	 
--     -- Error	 
--     -- Critical	 
--     -- Alert	 
--     -- Emergency
--   }

---------------------------------------------------

runTraceEmulator :: P.IO ()
runTraceEmulator = do
    P.putStrLn "--------------"
    P.putStrLn "Trace Emulator"
    P.putStrLn "--------------"
    let 
        run' = do 
            P.putStrLn "--------------"
            P.putStrLn "MENU"
            P.putStrLn "--------------"
            P.putStrLn "1 - Create and Run"
            P.putStrLn "0 - Exit"
            P.putStrLn "--------------"
            option <- P.getLine  
            P.putStrLn "--------------"
            case option of
                "1" -> do
                    P.putStrLn "Create and Run"
                    P.putStrLn "--------------"
                    TraceEmulator.runEmulatorTraceIO' DataDefault.def emulatorConfig emulatorCreateAndRun
                    run' 
                
                "0" -> do
                    P.putStrLn "Exiting..."
                _ -> do
                    P.putStrLn "Invalid option"
                    run' 
    run' 

---------------------------------------------------

emulatorCreateAndRun ::TraceEmulator.EmulatorTrace ()
emulatorCreateAndRun = do
    
    !h1 <- TraceEmulator.activateContractWallet (WalletEmulator.knownWallet 1) OffChain.endpoints

    let
        nombreProtocol = "MarketNFT"
        basePathFiles = "export/TraceEmulator"

    let 
        !w1 = WalletEmulator.walletToMockWallet' (WalletEmulator.knownWallet 1)
        !add = WalletEmulator.ownAddress $ WalletEmulator.fromMockWallet  w1
    !utxos <- getUtxos $ Utils.cardanoAddressToAddress add

    let
        protocolID_TxOutRef = fst $ head utxos
    MonadExtras.logInfo $ "ProtocolID_TxOutRef: " ++ P.show protocolID_TxOutRef

    let
        !pkh = WalletEmulator.mockWalletPaymentPubKeyHash (WalletEmulator.knownWallet 1)
        !masters = [Ledger.unPaymentPubKeyHash pkh]
    ------------------------
    let   
        !pABParams = SystemIOUnsafe.unsafePerformIO $ Deploy.exportarScripts basePathFiles nombreProtocol masters protocolID_TxOutRef 

    ------------------------
    emulatorRun pABParams h1
 
---------------------------------------------------

emulatorRun :: OffChain.PABParams -> TraceEmulator.ContractHandle () OffChain.ValidatorSchema DataText.Text -> TraceEmulator.EmulatorTrace ()
emulatorRun pABParams h1 = do

    let 
        
        !pkh = WalletEmulator.mockWalletPaymentPubKeyHash (WalletEmulator.knownWallet 1)
             
        pABPrepareParams = OffChain.PABPrepareProtocolParams{
                OffChain.ppParams = pABParams,
                OffChain.ppAldeaPkh  = Ledger.unPaymentPubKeyHash pkh,
                OffChain.ppAldeaPct = 1,
                OffChain.ppProtocolPkh = Ledger.unPaymentPubKeyHash pkh,
                OffChain.ppProtocolPct= 1
            }
            
    TraceEmulator.callEndpoint @"prepareProtocol" h1 pABPrepareParams
    Monad.void $ TraceEmulator.waitNSlots 10

    -- TraceEmulator.callEndpoint @"scriptAdd" h1 T.PABScriptAddParams { psapPABParams = pABParams}
    -- Monad.void $ TraceEmulator.waitNSlots 10

    -- TraceEmulator.callEndpoint @"scriptsDelete" h1 T.PABScriptsDeleteParams { psdpPABParams = pABParams}
    -- Monad.void $ TraceEmulator.waitNSlots 10

    -- TODO : agergar toda la ejecucion necesaria

    -- TraceEmulator.callEndpoint @"datumUpdate" h1 T.PABDatumUpdateParams { pdupPABParams = pABParams}
    -- Monad.void $ TraceEmulator.waitNSlots 4

    -- TraceEmulator.callEndpoint @"scriptAdd" h1 T.PABScriptAddParams { psapPABParams = pABParams}
    -- Monad.void $ TraceEmulator.waitNSlots 4
    -- TraceEmulator.callEndpoint @"depositsCreate" h1 T.PABDepositsCreateParams { pdcpPABParams = pABParams}
    -- Monad.void $ TraceEmulator.waitNSlots 4

---------------------------------------------------
