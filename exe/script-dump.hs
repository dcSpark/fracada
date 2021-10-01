
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import Fracada

import          Plutus.V1.Ledger.Value

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 4 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "script-dump <NFT currency symbol> <NFT token name> <Fraction token name> <number of fractions>"
  else 
    do 
      let       
        nftCurrencySymbol = BS.pack $ args!!0
        nftTokenName = BS.pack $ args!!1
        fractionTokenName = BS.pack $ args!!2
        numberOfFractions = (read $ args!!3 )::Integer
        validatorname = "validator.plutus"
        mintingname = "minting.plutus"
        scriptnum = 42
        nft = AssetClass (CurrencySymbol ( nftCurrencySymbol), TokenName nftTokenName)
        fractionToken = Plutus.TokenName fractionTokenName
        appliedValidatorScript =fractionValidatorScript nft

        validatorAsCbor = serialise appliedValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs
        appliedMintingPolicy = mintFractionTokensPolicy nft numberOfFractions fractionToken

        mintingAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintingPolicy
        mintingAsCbor = serialise mintingAsValidator
        mintingScriptShortBs = SBS.toShort . LB.toStrict $ mintingAsCbor
        mintingScript = PlutusScriptSerialised mintingScriptShortBs

      putStrLn $ "Writing output to: " ++ validatorname
      writePlutusScript scriptnum validatorname validatorScript validatorShortBs

      putStrLn $ "Writing output to: " ++ mintingname
      writePlutusScript scriptnum mintingname mintingScript mintingScriptShortBs


writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()


