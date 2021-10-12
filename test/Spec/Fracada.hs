{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Fracada (
    useCaseTests
    , successFullFractionalizationTrace
    ) where

import           Control.Monad              hiding (fmap)
import qualified Ledger
import           Ledger.Value               as Value
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator      as Trace
import           Test.Tasty

import qualified Fracada

-- Contracts' parameters

-- for EmulatorTrace it must be a 28-byte length ByteString
nftSymbol :: Ledger.CurrencySymbol
nftSymbol = currencySymbol "0123456789012345678901234567"

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
        
        , checkPredicate "Can lock NFT and mint fractional tokens"
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
