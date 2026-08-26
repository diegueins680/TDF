{-# LANGUAGE OverloadedStrings #-}

module TDF.AuthSpec (spec) where

import qualified Data.Text     as T
import           Test.Hspec    (Spec, describe, it, shouldBe)

import           TDF.Auth      (resolveUsernameFromLabel)

spec :: Spec
spec = describe "resolveUsernameFromLabel" $ do
  it "extracts username from password-login label" $
    resolveUsernameFromLabel "password-login:user@example.com"
      `shouldBe` Just "user@example.com"

  it "extracts username from password-reset label with surrounding whitespace" $
    resolveUsernameFromLabel "  password-reset:  reset@example.com  "
      `shouldBe` Just "reset@example.com"

  it "removes extraneous internal whitespace around username" $
    resolveUsernameFromLabel "password-login:   spaced@example.com   "
      `shouldBe` Just "spaced@example.com"

  it "returns Nothing for unsupported prefixes" $
    resolveUsernameFromLabel "api-token:other@example.com"
      `shouldBe` Nothing

  it "returns Nothing when no username is present" $ do
    resolveUsernameFromLabel "password-login:   "
      `shouldBe` Nothing
    resolveUsernameFromLabel T.empty
      `shouldBe` Nothing
