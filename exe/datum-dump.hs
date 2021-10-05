
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
import qualified Data.ByteString.Base16 as B16

import Fracada

import          Plutus.V1.Ledger.Value


import Data.Aeson
import Cardano.Api
import           Ledger           (pubKeyHash, PubKey(..), datumHash)
import Plutus.V1.Ledger.Api
import Data.Hex

main :: IO ()
main = do
  let       
    nftCurrencySymbol = BS.pack $ "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7" --args!!0
    nftTokenName = BS.pack $ "" -- args!!1
    fractionTokenName = BS.pack $ "FracadaToken" -- args!!2
    numberOfFractions = 10 -- (read $ args!!3 )::Integer
    (Right nftcs) = B16.decode nftCurrencySymbol
    nft = AssetClass (CurrencySymbol  nftcs, TokenName nftTokenName)
    fractionToken = Plutus.TokenName fractionTokenName
    
    pk :: PubKey
    pk = PubKey $ LedgerBytes $ B16.encode "addr_test1qqr585tvlc7ylnqvz8pyqwauzrdu0mxag3m7q56grgmgu7sxu2hyfhlkwuxupa9d5085eunq2qywy7hvmvej456flknswgndm3" 

    datum =FractionNFTDatum{ tokensClass= nft, totalFractions = 0, owner = pubKeyHash pk}
    dHash = datumHash $ Datum $ toBuiltinData datum
    datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
    encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode) 

  putStrLn $ "encoded datum: " ++ show encoded
  putStrLn $ "datum hash: " ++ show dHash
