{-# LANGUAGE OverloadedStrings #-}

module Main(
    main
    ) where

import qualified Spec.Fracada

import           Test.Tasty
import           Test.Tasty.Hedgehog (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain useCaseTests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

useCaseTests :: TestTree
useCaseTests = localOption limit $ testGroup "use cases" [
    Spec.Fracada.useCaseTests
    ]
