{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified BigQuery.TypesSpec as Types
import           Relude
import           Test.Hspec


-- * Top-level tests
------------------------------------------------------------------------------
-- | Combined tests.
specs :: Spec
specs  = Types.spec


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = hspec specs
