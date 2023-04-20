{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
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
module MarketNFTV2.Types where
--------------------------------------------------------------------------------
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                        as GHCGenerics (Generic)
-- import qualified Ledger    
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Schema                              (ToSchema)
--------------------------------------------------------------------------------
import qualified Text.Hex                                      as TextHex
import qualified Utils                       
import qualified Data.Maybe as DataMaybe
import qualified PlutusTx.Builtins.Class as TxBuiltinsClass
--------------------------------------------------------------------------------

type NFT = LedgerValue.AssetClass
type WalletPaymentPKH = LedgerApiV2.PubKeyHash
type StakeCredentialPubKeyHash = LedgerApiV2.PubKeyHash

--------------------------------------------------------------------------------
data DatumMarket = DatumMarket
    { 
        dSeller :: WalletPaymentPKH, 
        dSellerStakeCredential   :: Maybe StakeCredentialPubKeyHash, 
        dNFT :: NFT,
        dPriceADA  :: Maybe Integer,
        dPriceALDEA  :: Maybe Integer,
        dMinADA :: Integer
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

instance P.Eq DatumMarket where
    {-# INLINABLE (==) #-}
    a == b =    (dSeller a == dSeller b) &&
                (dSellerStakeCredential a == dSellerStakeCredential b) &&
                (dNFT a == dNFT b) &&
                (dPriceADA a == dPriceADA b) &&
                (dPriceALDEA a == dPriceALDEA b) &&
                (dMinADA a == dMinADA b)

PlutusTx.makeIsDataIndexed ''DatumMarket [ 
        ('DatumMarket, 0)
    ]

PlutusTx.makeLift ''DatumMarket

--------------------------------------------------------------------------------

data DatumProtocol = DatumProtocol
    { 
        aldeaPkh :: WalletPaymentPKH,
        aldeaStakeCredential :: Maybe StakeCredentialPubKeyHash,
        aldeaPct :: Integer,
        protocolPkh :: WalletPaymentPKH,
        protocolStakeCredential :: Maybe StakeCredentialPubKeyHash,
        protocolPct :: Integer
    } deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema)

instance P.Eq DatumProtocol where
    {-# INLINABLE (==) #-}
    a == b =    (aldeaPkh a == aldeaPkh b) &&
                (aldeaStakeCredential a == aldeaStakeCredential b) &&
                (aldeaPct a == aldeaPct b) &&
                (protocolPkh a == protocolPkh b) &&
                (protocolStakeCredential a == protocolStakeCredential b) &&
                (protocolPct a == protocolPct b)

PlutusTx.makeIsDataIndexed ''DatumProtocol [ 
        ('DatumProtocol, 0)
    ]

PlutusTx.makeLift ''DatumProtocol

--------------------------------------------------------------------------------

data NFTBuy = NFTBuy {
        nNFT :: NFT,
        nSwPayADA :: Integer
    }
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema)

instance Eq NFTBuy where
    {-# INLINABLE (==) #-}
    i1 == i2 = nNFT i1 == nNFT i2 && nSwPayADA i1 == nSwPayADA i2 

-- PlutusTx.makeLift ''NFTBuy
PlutusTx.makeIsDataIndexed ''NFTBuy [ 
        ('NFTBuy, 0)
    ]

-- data RedeemerSellerGetBackNFTTypo  = RedeemerSellerGetBackNFTTypo { 
        
--     } 
--     deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

-- instance Eq RedeemerSellerGetBackNFTTypo where
--     {-# INLINABLE (==) #-}
--     r1 == r2 =  True

-- PlutusTx.makeIsDataIndexed ''RedeemerSellerGetBackNFTTypo [ 
--         ('RedeemerSellerGetBackNFTTypo, 0)
--     ]

newtype RedeemerBuyerBuyNFTTypo  = RedeemerBuyerBuyNFTTypo { 
        rbbNFTsBuy :: [NFTBuy]
    } 
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, Schema.ToSchema, DataOpenApiSchema.ToSchema)

instance Eq RedeemerBuyerBuyNFTTypo where
    {-# INLINABLE (==) #-}
    r1 == r2 =  rbbNFTsBuy r1 == rbbNFTsBuy r2 

PlutusTx.makeIsDataIndexed ''RedeemerBuyerBuyNFTTypo [ 
        ('RedeemerBuyerBuyNFTTypo, 0)
    ]

data RedeemerMarket = 
    RedeemerSellerGetBackNFT | 
    RedeemerBuyerBuyNFT RedeemerBuyerBuyNFTTypo
    deriving (P.Eq, P.Show, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

instance Eq RedeemerMarket where
    {-# INLINABLE (==) #-}
    -- RedeemerSellerGetBackNFT  rmf1 == RedeemerSellerGetBackNFT  rmf2  = rmf1 == rmf2
    RedeemerSellerGetBackNFT   == RedeemerSellerGetBackNFT  = True 
    RedeemerBuyerBuyNFT rmf1 == RedeemerBuyerBuyNFT rmf2 = rmf1 == rmf2
    _ == _ = False

PlutusTx.makeIsDataIndexed ''RedeemerMarket [ 
        ('RedeemerSellerGetBackNFT, 0),
        ('RedeemerBuyerBuyNFT, 1)
    ]

-- PlutusTx.makeLift ''RedeemerMarket

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

-- aldea_CS :: LedgerApiV2.CurrencySymbol
aldea_BBS :: P.String
aldea_BBS = "828f7d2975d3230649a1b012f2f17ff8ee95f37ff32cd217a38dde74"
    --PREVIEW "828f7d2975d3230649a1b012f2f17ff8ee95f37ff32cd217a38dde74"
    --PAB SIMULATOR POLICY 3 o 6 "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759" 
    -- MAINNET "37f9b0f7e6a46d03b46c8f167f3e8f27008bbfe68b2908d34bd5a673"

aldea_CS :: LedgerApiV2.CurrencySymbol
aldea_CS =  LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin $ DataMaybe.fromJust $ TextHex.decodeHex $ Utils.stringToStrictText aldea_BBS

aldea_TN :: LedgerApiV2.TokenName
aldea_TN = LedgerApiV2.TokenName "ALDEA"

minComission :: Integer
minComission = 1000000

--------------------------------------------------------------------------------
