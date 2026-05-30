module Main
  ( main
  ) where

import Test.Tasty
  ( defaultMain
  , testGroup
  )

import Crypto.Kata.FieldSpec
  ( fieldSpec
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "advanced-crypto-tiny-programs"
        [ fieldSpec
        ]
    )
