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
{-# LANGUAGE LambdaCase          #-}

module Fracada where

import           Prelude                ( String, show, Show)
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Contexts                   as Validation
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, NonEmpty(..) )
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions, ensureKnownCurrencies)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           GHC.Generics         (Generic)
import           Data.Aeson           (ToJSON, FromJSON)


data FractionNFTDatum = FractionNFTDatum {
      tokensClass     :: AssetClass,
      totalFractions  :: Integer
    } deriving (Generic, Show)

PlutusTx.makeLift ''FractionNFTDatum
PlutusTx.makeIsDataIndexed ''FractionNFTDatum [('FractionNFTDatum,0)]

-- | Datum and redeemer parameter types for fractioning script
data Fractioning
instance Scripts.ValidatorTypes Fractioning where
    type instance RedeemerType Fractioning = ()
    type instance DatumType Fractioning = FractionNFTDatum

{-# INLINABLE datumToData #-}
datumToData :: (FromData a) => Datum -> Maybe a
datumToData datum = fromBuiltinData (getDatum datum)

{-# INLINABLE fractionNftValidator #-}
fractionNftValidator :: AssetClass -> FractionNFTDatum -> () -> ScriptContext -> Bool
fractionNftValidator nftAsset FractionNFTDatum{tokensClass, totalFractions} _ ctx =
  let
      txInfo = scriptContextTxInfo ctx
        
      -- extract signer of this transaction, assume is only one
      [sig] = txInfoSignatories txInfo 

      nftIsPaidToOwner = assetClassValueOf (Validation.valuePaidTo txInfo sig ) nftAsset == 1

      forgedTokens = assetClassValueOf (txInfoMint txInfo) tokensClass
      tokensBurnt = (forgedTokens == negate totalFractions)  && forgedTokens /= 0 
  in
      traceIfFalse "NFT not paid to owner" nftIsPaidToOwner &&
      traceIfFalse "Tokens not burn" tokensBurnt


fractionNftValidatorInstance ::  AssetClass -> Scripts.TypedValidator Fractioning
fractionNftValidatorInstance asset = Scripts.mkTypedValidator @Fractioning
    ($$(PlutusTx.compile [||  fractionNftValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode asset)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @FractionNFTDatum @()

fractionNftValidatorHash :: AssetClass -> ValidatorHash
fractionNftValidatorHash = Scripts.validatorHash . fractionNftValidatorInstance

fractionValidatorScript :: AssetClass -> Validator
fractionValidatorScript = Scripts.validatorScript . fractionNftValidatorInstance

fractionNftValidatorAddress :: AssetClass -> Address
fractionNftValidatorAddress = Ledger.scriptAddress . fractionValidatorScript


{-# INLINABLE mintFractionTokens #-}
mintFractionTokens :: ValidatorHash -> AssetClass -> Integer -> TokenName -> BuiltinData -> ScriptContext -> Bool
mintFractionTokens fractionNFTScript asset numberOfFractions fractionTokenName _ ctx =

  let
    info = scriptContextTxInfo ctx
    mintedAmount = case flattenValue (txInfoMint info) of
        [(cs, fractionTokenName', amt)] | cs == ownCurrencySymbol ctx && fractionTokenName' == fractionTokenName -> amt 
        _                                                           -> 0
  in
    if mintedAmount > 0 then
      let 
        lockedByNFTfractionScript = valueLockedBy info fractionNFTScript 
        assetIsPaid = assetClassValueOf lockedByNFTfractionScript asset == 1 
      in
        traceIfFalse "Asset not paid" assetIsPaid           &&
        traceIfFalse "wrong fraction tokens minted" ( mintedAmount == numberOfFractions)
    else
      let
        -- extract signer of this transaction, assume is only one
        [sig] = txInfoSignatories info 
        assetIsReturned = assetClassValueOf (Validation.valuePaidTo info sig ) asset == 1  
      in
        traceIfFalse "Asset not returned" assetIsReturned           &&
        traceIfFalse "wrong fraction tokens burned" ( mintedAmount == negate numberOfFractions)



mintFractionTokensPolicy :: AssetClass -> Integer -> TokenName -> Scripts.MintingPolicy
mintFractionTokensPolicy asset numberOfFractions fractionTokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \validator' asset' numberOfFractions' fractionTokenName' -> Scripts.wrapMintingPolicy $ mintFractionTokens validator' asset' numberOfFractions' fractionTokenName' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode ( fractionNftValidatorHash asset)
    `PlutusTx.applyCode`
    PlutusTx.liftCode asset
    `PlutusTx.applyCode`
    PlutusTx.liftCode numberOfFractions
    `PlutusTx.applyCode`
    PlutusTx.liftCode fractionTokenName

curSymbol ::  AssetClass -> Integer -> TokenName -> CurrencySymbol
curSymbol asset numberOfFractions fractionTokenName = scriptCurrencySymbol $ mintFractionTokensPolicy asset numberOfFractions fractionTokenName

data ToFraction = ToFraction
    { nftAsset :: !AssetClass
    , fractions   :: !Integer
    , fractionTokenName    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FracNFTSchema = Endpoint "1-fractionNFT" ToFraction
    .\/ Endpoint "2-returnNFT" AssetClass



extractData :: (PlutusTx.FromData a) => ChainIndexTxOut -> Contract w s Text a
extractData o =
  case o of
      PublicKeyChainIndexTxOut {} ->
        throwError "no datum for a txout of a public key address"
      ScriptChainIndexTxOut { _ciTxOutDatum } -> do
        (Datum e) <- either getDatum pure _ciTxOutDatum
        maybe (throwError "datum hash wrong type")
              pure
              (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d


fractionNFT :: ToFraction -> Contract w FracNFTSchema Text ()
fractionNFT ToFraction {nftAsset, fractions, fractionTokenName} = do
  -- pay nft to contract
  -- pay minted tokens back to signer
    pk    <- Contract.ownPubKey
    let
      --find the minting script instance
      mintingScript = mintFractionTokensPolicy nftAsset fractions fractionTokenName

      -- define the value to mint (amount of tokens) and be paid to signer  
      currency = scriptCurrencySymbol mintingScript  
      tokensToMint =  Value.singleton currency fractionTokenName fractions
      payBackTokens = mustPayToPubKey (pubKeyHash pk) tokensToMint
      -- value of NFT 
      valueToScript = assetClassValue nftAsset 1 
      -- keep the minted amount and asset class in the datum
      datum = Datum $ toBuiltinData FractionNFTDatum{ tokensClass= assetClass currency fractionTokenName, totalFractions = fractions}
      
      --build the constraints and submit the transaction
      validator = fractionValidatorScript nftAsset
      lookups = Constraints.mintingPolicy mintingScript  <> 
                Constraints.otherScript validator              
      tx      = Constraints.mustMintValue tokensToMint <> 
                Constraints.mustPayToOtherScript (fractionNftValidatorHash nftAsset) datum valueToScript <>
                payBackTokens
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show fractions)    

returnNFT :: AssetClass -> Contract w FracNFTSchema Text ()
returnNFT nftAsset = do
  -- pay nft to signer
  -- burn tokens
    pk    <- Contract.ownPubKey
    utxos <- utxosAt $ fractionNftValidatorAddress nftAsset
    let
      -- declare the NFT value
      valueToWallet = assetClassValue nftAsset 1
      -- find the UTxO that has the NFT we're looking for
      utxos' = Map.filter (\v -> 1 == assetClassValueOf (_ciTxOutValue v) nftAsset ) utxos
      (nftRef,nftTx) = head $ Map.toList utxos'
      -- use the auxiliary extractData function to get the datum content
    -- assuming that all the fraction tokens are in the owner's `ownPubkey` address. For tracing it is good enough,
    -- though for real-use-cases it is more nuanced, as the owner can have them on different
    -- UTxOs.
    fracTokenUtxos <- utxosAt (Ledger.pubKeyAddress pk)
    FractionNFTDatum {tokensClass, totalFractions } <- extractData nftTx
    let
      -- declare the fractional tokens to burn
      (_, fractionTokenName) = unAssetClass tokensClass
      tokensCurrency =  curSymbol nftAsset totalFractions fractionTokenName
      tokensToBurn =  Value.singleton tokensCurrency fractionTokenName $ negate totalFractions

      -- build the constraints and submit
      validator = fractionValidatorScript nftAsset
      lookups = Constraints.mintingPolicy (mintFractionTokensPolicy nftAsset totalFractions fractionTokenName)  <>
                Constraints.otherScript validator <>
                Constraints.unspentOutputs utxos' <>
                Constraints.unspentOutputs fracTokenUtxos <>
                Constraints.ownPubKeyHash (pubKeyHash pk)

      tx      = Constraints.mustMintValue tokensToBurn <>
                Constraints.mustSpendScriptOutput nftRef ( Redeemer $ toBuiltinData () ) <>
                Constraints.mustPayToPubKey (pubKeyHash pk) valueToWallet

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "burnt %s" (show totalFractions)

endpoints :: Contract () FracNFTSchema Text ()
endpoints = forever
                $ handleError logError
                $ awaitPromise
                $ fractionNFT' `select` burn'
  where
    fractionNFT' = endpoint @"1-fractionNFT" $ fractionNFT
    burn' = endpoint @"2-returnNFT" $ returnNFT

mkSchemaDefinitions ''FracNFTSchema

nfts :: KnownCurrency
nfts = KnownCurrency (ValidatorHash "f") "Token" (TokenName "NFT" :| [])

mkKnownCurrencies ['nfts]
