{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Fracada (
    useCaseTests
    , successFullFractionalizationTrace
    ) where

-- import           Control.Monad              (void)
import           Control.Monad              hiding (fmap)

-- import qualified Data.Map                   as Map
-- import           Data.Default               (Default (..))

import qualified Ledger
-- import qualified Ledger.Ada                 as Ada
import           Ledger.Value               as Value
-- import           Plutus.Contract
import           Plutus.Contract.Test

-- import           Fracada                    (FractionNFTDatum)
import qualified Fracada

-- import           Plutus.Trace.Emulator      (EmulatorTrace, EmulatorConfig)
import qualified Plutus.Trace.Emulator      as Trace

import           Test.Tasty



-- Contracts' parameters

nftSymbol :: Ledger.CurrencySymbol
nftSymbol = currencySymbol "f"

nftName :: Ledger.TokenName
nftName = TokenName "fractionNFT"

nftAssetClass :: Ledger.AssetClass
nftAssetClass = Value.assetClass nftSymbol nftName

fractionTokenName :: Ledger.TokenName
fractionTokenName  = "fractionToken"

fractions :: Integer
fractions = 100

useCaseTests :: TestTree
useCaseTests = 
    let contract = Fracada.endpoints in
        testGroup "fracada"
        [ checkPredicate "Expose '1-fractionNFT' and '2-returnNFT' endpoints"
        (endpointAvailable @"1-fractionNFT" contract (Trace.walletInstanceTag w1)
        .&&. endpointAvailable @"2-returnNFT" contract (Trace.walletInstanceTag w2)
        ) $ void (Trace.activateContractWallet w1 contract)
        
        , checkPredicate "Can lock NFT and mint fractionalize tokens"
        assertNoFailedTransactions
        successFullFractionalizationTrace
        
        ]

successFullFractionalizationTrace :: Trace.EmulatorTrace ()
successFullFractionalizationTrace = do
    h1 <- Trace.activateContractWallet (knownWallet 1) Fracada.endpoints
    -- h2 <- Trace.activateContractWallet (knownWallet 2) Fracada.endpoints

    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"1-fractionNFT" h1 Fracada.ToFraction { Fracada.nftAsset = nftAssetClass
                                                                , Fracada.fractions = fractions
                                                                , Fracada.fractionTokenName = fractionTokenName }
    void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"2-returnNFT" h2 nftAssetClass
    -- void $ Trace.waitNSlots 1