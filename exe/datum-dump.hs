
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Plutus.V1.Ledger.Api       as Plutus
import qualified Data.ByteString.Base16     as B16
import           Fracada
import           Plutus.V1.Ledger.Value
import           Data.Aeson
import           Ledger                     (pubKeyHash, PubKey(..), datumHash)
import           Plutus.V1.Ledger.Api
import           Data.String                (IsString (..))

-- test data
-- nftCurrencySymbol = fromString  "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7" 
-- nftTokenName =  "" 
-- fractionTokenName =  "FracadaToken" 
-- numberOfFractions = 10 :: Integer 
-- address = "addr_test1qqeg3tkj4sets754a4jguuq5tzrfzfkgw5ph0wekrkym343m8nmfxuxr4tlwjw0xklxpmf78ef0vertg5htkzzawve4qv057ql" 

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 4 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "datum-dump <NFT currency symbol> <NFT token name> <number of fractions> <owner address>"
  else 
    do 
      let       
        [nftSymbol, nftTokenName', numberOfFractions', ownerAddress] = args
        nftCurrencySymbol = fromString nftSymbol
        nftTokenName = fromString nftTokenName' 
        numberOfFractions = (read numberOfFractions' )::Integer
  
        nft = AssetClass (nftCurrencySymbol, nftTokenName)
        pk :: PubKey
        pk = PubKey $ fromBytes $ B16.encode $ fromString ownerAddress

        datum =FractionNFTDatum{ tokensClass= nft, totalFractions = numberOfFractions, owner = pubKeyHash pk}
        dHash = datumHash $ Datum $ toBuiltinData datum
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData datum
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode) 

      putStrLn $ "encoded datum: " ++ show encoded
      putStrLn $ "datum hash: " ++ show dHash

