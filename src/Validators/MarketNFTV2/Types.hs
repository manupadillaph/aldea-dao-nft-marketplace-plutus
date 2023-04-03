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
{- HLINT ignore "Use camelCase" -}
--------------------------------------------------------------------------------
module Validators.MarketNFTV2.Types where
--------------------------------------------------------------------------------
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Ledger    
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
--------------------------------------------------------------------------------
type NFT = LedgerValue.AssetClass
type WalletPaymentPKH = LedgerApiV2.PubKeyHash --LedgerApiV2.PubKeyHash

--------------------------------------------------------------------------------
data DatumMarket = DatumMarket
    { 
        dSeller :: LedgerApiV2.PubKeyHash, 
        dNFT :: NFT,
        dPriceADA  :: Maybe Integer,
        dPriceALDEA  :: Maybe Integer
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

instance P.Eq DatumMarket where
    {-# INLINABLE (==) #-}
    a == b =    (dSeller a == dSeller b) &&
                (dNFT a == dNFT b) &&
                (dPriceADA a == dPriceADA b) &&
                (dPriceALDEA a == dPriceALDEA b) 

PlutusTx.makeIsDataIndexed ''DatumMarket [ 
        ('DatumMarket, 0)
    ]

PlutusTx.makeLift ''DatumMarket

--------------------------------------------------------------------------------

data DatumProtocol = DatumProtocol
    { 
        aldeaPkh :: LedgerApiV2.PubKeyHash,
        aldeaPct :: Integer,
        protocolPkh :: LedgerApiV2.PubKeyHash,
        protocolPct :: Integer
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

instance P.Eq DatumProtocol where
    {-# INLINABLE (==) #-}
    a == b = (aldeaPkh a == aldeaPkh b) &&
             (aldeaPct a == aldeaPct b) &&
             (protocolPkh a == protocolPkh b) &&
             (protocolPct a == protocolPct b)

PlutusTx.makeIsDataIndexed ''DatumProtocol [ 
        ('DatumProtocol, 0)
    ]

PlutusTx.makeLift ''DatumProtocol

--------------------------------------------------------------------------------

data RedeemerMarket = 
    RedeemerSellerGetBackNFT |
    RedeemerBuyerBuyNFT
    
    deriving P.Show


PlutusTx.makeIsDataIndexed ''RedeemerMarket [ 
        ('RedeemerSellerGetBackNFT, 0),
        ('RedeemerBuyerBuyNFT, 1)
    ]

PlutusTx.makeLift ''RedeemerMarket

--------------------------------------------------------------------------------

data RedeemerProtocol = 
    RedeemerUpdateDatum 
    deriving P.Show

PlutusTx.makeIsDataIndexed ''RedeemerProtocol [ 
        ('RedeemerUpdateDatum, 0)
    ]

PlutusTx.makeLift ''RedeemerProtocol

--------------------------------------------------------------------------------
type TxOut_With_Datum    = (LedgerApiV2.TxOut, DatumMarket)
type TxOut_Value_And_Datum     = (LedgerApiV2.Value, DatumMarket)
--------------------------------------------------------------------------------

protocolID_TN :: LedgerApiV2.TokenName
protocolID_TN = LedgerApiV2.TokenName "P"

aldea_CS :: LedgerApiV2.CurrencySymbol
aldea_CS = LedgerApiV2.CurrencySymbol "ffaa"

aldea_TN :: LedgerApiV2.TokenName
aldea_TN = LedgerApiV2.TokenName "ALDEA"

minComission :: Integer
minComission = 1000000
--------------------------------------------------------------------------------
