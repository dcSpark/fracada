
{-# LANGUAGE OverloadedStrings #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus

import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Short      as SBS

import           Ledger                     (datumHash)


import           Fracada

import           Data.Aeson
import           Data.String                (IsString (..))
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Value

-- test data
-- nftCurrencySymbol = fromString  "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7"
-- nftTokenName =  ""
-- fractionTokenName =  "FracadaToken"
-- numberOfFractions = 10 :: Integer
-- nft = AssetClass (nftCurrencySymbol, nftTokenName)
-- fractionToken = Plutus.TokenName fractionTokenName

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 4 then
    do
      putStrLn "Usage:"
      putStrLn "script-dump <NFT currency symbol> <NFT token name> <Fraction token name> <number of fractions>"
  else
    do
      let
        [nftSymbol, nftTokenName, fractionTokenName, numberOfFractions] = args

        numberOfFractions' = read numberOfFractions

        nftCurrencySymbol = fromString nftSymbol
        nftTokenName' = fromString nftTokenName
        nft = AssetClass (nftCurrencySymbol, nftTokenName')

        fractionCurrencySymbol = curSymbol nft numberOfFractions' fractionTokenName'
        fractionTokenName' = fromString fractionTokenName
        token = AssetClass (fractionCurrencySymbol, fractionTokenName')

        validatorname = "validator.plutus"
        mintingname = "minting.plutus"

        scriptnum = 42

        -- fractionToken = Plutus.TokenName fractionTokenName'
        appliedValidatorScript = fractionValidatorScript nft

        validatorAsCbor = serialise appliedValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs
        appliedMintingPolicy = mintFractionTokensPolicy nft numberOfFractions' fractionTokenName'

        mintingAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintingPolicy
        mintingAsCbor = serialise mintingAsValidator
        mintingScriptShortBs = SBS.toShort . LB.toStrict $ mintingAsCbor
        mintingScript = PlutusScriptSerialised mintingScriptShortBs

        datum =FractionNFTDatum{ tokensClass = token, totalFractions = numberOfFractions'}
        dHash = datumHash $ Datum $ toBuiltinData datum
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode)

      putStrLn $ "Writing Validator script to: " ++ validatorname
      writePlutusScript scriptnum validatorname validatorScript validatorShortBs

      putStrLn $ "validator hash " ++ show (fractionNftValidatorHash nft)

      putStrLn $ "Writing Minting script to: " ++ mintingname
      writePlutusScript scriptnum mintingname mintingScript mintingScriptShortBs

      putStrLn $ "minting hash " ++  show fractionCurrencySymbol

      putStrLn $ "encoded datum: " ++ show encoded
      putStrLn $ "datum hash: " ++ show dHash

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr   -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()


