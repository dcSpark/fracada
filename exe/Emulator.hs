{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

import           Data.Default          (Default (..))
import           Plutus.Trace.Emulator
import           Prelude               (IO)

import           Spec.Fracada

main :: IO ()
main = do
    runEmulatorTraceIO' def fracadaEmulatorConfig successFulFractionalizationTrace
    runEmulatorTraceIO' def fracadaEmulatorConfig unSuccessFulFractionalizationTrace
