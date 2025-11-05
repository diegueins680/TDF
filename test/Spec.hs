module Main (main) where

import Test.Hspec (hspec)

import qualified TDF.AuthSpec

main :: IO ()
main = hspec TDF.AuthSpec.spec
