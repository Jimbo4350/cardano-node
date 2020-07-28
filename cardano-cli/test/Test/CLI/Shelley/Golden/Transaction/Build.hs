{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.Transaction.Build
  ( golden_shelleyTransactionBuild
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyTransactionBuild :: Property
golden_shelleyTransactionBuild = OP.propertyOnce $ OP.workspace "tmp/transaction-build" $ \tempDir -> do
  -- Use the same (faked) TxIn for both transactions.
  let txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

  -- Using a Shelley output address
  let txOut = "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

  txBodyOutFile <- OP.noteTempFile tempDir "tx-body-out"

  void . OP.noteEvalM $ OP.execCardanoCLI
    [ "shelley","transaction","build-raw"
    , "--tx-in", txIn
    , "--tx-out", txOut
    , "--ttl", "60"
    , "--fee", "12"
    , "--tx-body-file", txBodyOutFile
    ]

  OP.assertFileOccurences 1 "TxUnsignedShelley" txBodyOutFile

  OP.assertEndsWithSingleNewline txBodyOutFile
