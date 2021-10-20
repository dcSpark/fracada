{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           PlutusBenchmark.Common                   (Term, getConfig, getDataDir, unDeBruijnAnonTerm)
import           PlutusBenchmark.NaturalSort

import qualified PlutusCore                               as PLC

import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

import           Criterion.Main
import           Criterion.Main.Options                   (Mode, parseWith)
import           Criterion.Types                          (Config (..))
import           Options.Applicative

import           Control.DeepSeq                          (force)
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BSL
import           Data.List                                (isPrefixOf)
import           Flat
import           System.Directory                         (listDirectory)
import           System.FilePath

-- Static directory for the flat plutus script's files. 
getScriptDirectory :: IO FilePath
getScriptDirectory = do
  root <- getDataDir
  return $ root </> "benchmark" </> "data"

quickPrefixes :: [String]
quickPrefixes = [ "fracada" ]

-- Given two lists of strings l and ps, return the elements of l which have any
-- element of ps as a prefix
withAnyPrefixFrom :: [String] -> [String] -> [String]
l `withAnyPrefixFrom` ps =
    concatMap (\p -> filter (isPrefixOf p) l) ps

loadFlat :: FilePath -> IO Term
loadFlat file = do
  contents <- BSL.fromStrict <$> BS.readFile file
  case unflat contents of
    Left e  -> errorWithoutStackTrace $ "Flat deserialisation failure for " ++ file ++ ": " ++ show e
    Right prog -> do
        let t = unDeBruijnAnonTerm $ UPLC.toTerm prog
        return $! force t
        -- `force` to try to ensure that deserialiation is not included in benchmarking time.

mkCekBM :: Term -> Benchmarkable
mkCekBM program = whnf (UPLC.unsafeEvaluateCekNoEmit PLC.defaultCekParameters) program

mkScriptBM :: FilePath -> FilePath -> Benchmark
mkScriptBM dir file =
    env (loadFlat $ dir </> file) $ \script -> bench (dropExtension file) $ mkCekBM script

-- Make benchmarks for the given files in the directory
mkBMs :: FilePath -> [FilePath] -> [Benchmark]
mkBMs dir files = map (mkScriptBM dir) files


----------------------- Main -----------------------

-- Extend the options to include `--quick`: see eg https://github.com/haskell/criterion/pull/206
data BenchOptions = BenchOptions
  { quick        :: Bool
  , otherOptions :: Mode  -- The standard options
  }

parseBenchOptions :: Config -> Parser BenchOptions
parseBenchOptions cfg = BenchOptions
  <$> switch
      (  short 'q'
      <> long "quick"
      <> help "Run only a small subset of the benchmarks")
  <*> parseWith cfg

parserInfo :: Config -> ParserInfo BenchOptions
parserInfo cfg =
    info (helper <*> parseBenchOptions cfg) $ header "Plutus Core validation benchmark suite"

{- Run the benchmarks.  You can run groups of benchmarks by typing things like
     `stack bench -- plutus-benchmark:validation --ba crowdfunding`
   or
     `cabal bench -- plutus-benchmark:validation --benchmark-options crowdfunding`.
-}
main :: IO ()
main = do
  cfg <- getConfig 20.0  -- Run each benchmark for at least 20 seconds.  Change this with -L or --timeout (longer is better).
  options <- execParser $ parserInfo cfg
  scriptDirectory <- getScriptDirectory
  files0 <- listDirectory scriptDirectory  -- Just the filenames, not the full paths
  let files1 = naturalSort $ filter (isExtensionOf ".flat") files0  -- Just in case there's anything else in the directory.
               -- naturalSort puts the filenames in a better order than Data.List.Sort
      files = if quick options then files1 `withAnyPrefixFrom` quickPrefixes else files1
      benchmarks = mkBMs scriptDirectory files
  runMode (otherOptions options) benchmarks
