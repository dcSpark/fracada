{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Fracada (
    useCaseTests
    , successFulFractionalizationTrace
    , unSuccessFulFractionalizationTrace
    , fracadaNft
    , fracadaEmulatorConfig
    ) where

import           Control.Lens
import           Control.Monad                  hiding (fmap)
import           Control.Monad.Freer.Extras.Log (LogLevel (..))
import           Data.Default                   as Default
import qualified Data.Map                       as Map
import qualified Ledger
import           Ledger.Ada                     as Ada
import           Ledger.Value                   as Value
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator          as Trace
import           Test.Tasty
import           Test.Tasty.HUnit               as HUnit

import           Fracada

-- Contracts' parameters

-- for EmulatorTrace it must be a 28-byte length ByteString
nftSymbol :: Ledger.CurrencySymbol
nftSymbol = currencySymbol "0123456789012345678901234567"

nftName :: Ledger.TokenName
nftName = TokenName "fracNFT"

fracadaNft :: Ledger.AssetClass
fracadaNft = Value.assetClass nftSymbol nftName

fracName :: Ledger.TokenName
fracName  = "fracToken"

fracs :: Integer
fracs = 100

toFraction :: ToFraction
toFraction = ToFraction {
        nftAsset = fracadaNft
        , fractions = fracs
        , fractionTokenName = fracName
    }

fracadaEmulatorConfig :: EmulatorConfig
fracadaEmulatorConfig =
  EmulatorConfig (Left $ Map.fromList [(w1, v1), (w2, v2)]) def def
  where
    -- The Wallet 1 contains the NFT to lock
    v1 = Ada.lovelaceValueOf 1000_000_000 <> assetClassValue fracadaNft 1
    v2 = Ada.lovelaceValueOf 1000_000_000

options :: CheckOptions
options = set emulatorConfig fracadaEmulatorConfig (set minLogLevel Debug defaultCheckOptions)
-- or (defaultCheckOptions & minLogLevel .~ Debug)  ^

useCaseTests :: TestTree
useCaseTests =
    let reasonableSize = 9000
        contract = Fracada.endpoints in
        testGroup "fracada"
        [ checkPredicate "Expose '1-fractionNFT' and '2-returnNFT' endpoints"
          (endpointAvailable @"1-fractionNFT" contract (walletInstanceTag w1)
          .&&. endpointAvailable @"2-returnNFT" contract (walletInstanceTag w2)
          ) $ void (activateContractWallet w1 contract)

        , checkPredicateOptions options "Can lock NFT, mint fractional tokens and unlock to any address"
          assertNoFailedTransactions
          successFulFractionalizationTrace

        , checkPredicateOptions options "Can lock NFT and mint fractional tokens, but fail to unlock"
          (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (Ledger.EvaluationError ["not enough signatures"] _) -> True; _ -> False  }))
          unSuccessFulFractionalizationTrace

        , HUnit.testCase ("The script size (" ++ (show reasonableSize) ++ ") is reasonable.")
          ( reasonable ( Fracada.fractionValidatorScript fracadaNft )
            reasonableSize
          )
        ]

successFulFractionalizationTrace :: EmulatorTrace ()
successFulFractionalizationTrace = do

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    void $ waitNSlots 1

    let
        -- all fraction tokens
        fracValue = Value.singleton (curSymbol fracadaNft fracs fracName) fracName fracs

    callEndpoint @"1-fractionNFT" h1 toFraction
    void $ waitNSlots 1

    -- pay minted fractions to the second wallet
    void $ payToWallet w1 w2 fracValue
    void $ waitNSlots 1

    -- unlocking to a custom wallet
    callEndpoint @"2-returnNFT" h2 fracadaNft
    void $ waitNSlots 1

unSuccessFulFractionalizationTrace :: EmulatorTrace ()
unSuccessFulFractionalizationTrace = do

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    void $ waitNSlots 1

    callEndpoint @"1-fractionNFT" h1 toFraction
    void $ waitNSlots 1

    -- unlocking should fail as the h2 does not have the required fraction Tokens
    callEndpoint @"2-returnNFT" h2 fracadaNft
    void $ waitNSlots 1
