{-# LANGUAGE DataKinds, NoImplicitPrelude, TypeFamilies, TypeOperators #-}

module Main where

import           GHC.TypeLits
import           Relude
import           Test.Hspec

import           Data.Google.Types


{-- }
data FakeScope0 :: Symbol
data FakeScope1 :: Symbol
data FakeScope2 :: Symbol
--}
type FakeScope0 = "fakest-of-scopes-0"
type FakeScope1 = "fakest-of-scopes-1"
type FakeScope2 = "fakest-of-scopes-2"

type ScopeSet0 = '[FakeScope0, FakeScope1] :: [Symbol]
type ScopeSet1 = '[FakeScope1, FakeScope2] :: [Symbol]


spec :: Spec
spec  = describe "Testbenches for Google monad and types" $ do

  context "Auth-scope tests" $ do

    it "can compile scopes-concatenation" $ do
      let scp0 = Proxy :: Proxy (Concat '[ScopeSet0, ScopeSet1])
          scp1 = Proxy :: Proxy '[FakeScope0, FakeScope1, FakeScope2]
      scp0 `shouldBe` scp1

    it "can compile scopes-intersection" $ do
      let scp0 = Proxy :: Proxy (Intersect ScopeSet0 ScopeSet1)
          scp1 = Proxy :: Proxy '[FakeScope1]
      scp0 `shouldBe` scp1


main :: IO ()
main  = hspec spec
