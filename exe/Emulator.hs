{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


import           Prelude                (IO)
import           Control.Monad          hiding (fmap)
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.Value           as Value
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Fracada


main :: IO ()
main = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    let 
        nft = AssetClass (currencySymbol "66", tokenName "NFT")
        toFraction = ToFraction
            { nftAsset = nft
            , fractions = 10
            , fractionTokenName = tokenName "Frac" 
            }
    callEndpoint @"1-lockNFT" h1 nft
    callEndpoint @"2-fractionNFT" h1 toFraction
    callEndpoint @"3-returnNFT" h2 nft
    void $ Emulator.waitNSlots 1