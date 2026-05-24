{-# LANGUAGE DataKinds #-}

module Crypto.Kata.FieldSpec
  ( fieldSpec
  ) where

import Crypto.Kata.Field

import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , NonNegative (NonNegative)
  , Positive (Positive)
  , Property
  , chooseInteger
  , counterexample
  , property
  , (===)
  , (==>)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.QuickCheck
  ( testProperty
  )

type F17 = F 17

fieldSpec :: TestTree
fieldSpec =
  testGroup
    "Kata 1: finite field laws"
    [ testGroup
        "canonical representation"
        [ testProperty "mkF returns values in [0, p)" prop_mkFCanonical
        , testProperty "mkF respects congruence modulo p" prop_mkFModulo
        ]
    , testGroup
        "additive structure"
        [ testProperty "addition is associative" prop_addAssociative
        , testProperty "addition is commutative" prop_addCommutative
        , testProperty "zero is additive identity" prop_addIdentity
        , testProperty "negation gives additive inverse" prop_addInverse
        , testProperty "subtraction agrees with addition of negation" prop_subAsAddNeg
        ]
    , testGroup
        "multiplicative structure"
        [ testProperty "multiplication is associative" prop_mulAssociative
        , testProperty "multiplication is commutative" prop_mulCommutative
        , testProperty "one is multiplicative identity" prop_mulIdentity
        , testProperty "non-zero elements have multiplicative inverse" prop_mulInverse
        ]
    , testGroup
        "field structure"
        [ testProperty "multiplication distributes over addition" prop_distributive
        , testProperty "division agrees with multiplication by inverse" prop_divAsMulInv
        ]
    , testGroup
        "exponentiation"
        [ testProperty "x^0 = 1" prop_powZero
        , testProperty "x^1 = x" prop_powOne
        , testProperty "x^(m+n) = x^m * x^n" prop_powAdd
        , testProperty "Fermat: non-zero x satisfies x^(p-1) = 1" prop_fermat
        ]
    ]

instance Arbitrary F17 where
  arbitrary = genF17

genF17 :: Gen F17
genF17 = mkF <$> chooseInteger (-10_000, 10_000)

isZero :: F17 -> Bool
isZero x = x == zero

prop_mkFCanonical :: Integer -> Property
prop_mkFCanonical n =
  let x = mkF @17 n
      v = unF x
   in counterexample ("unF x = " <> show v) $
        property (0 <= v && v < 17)

prop_mkFModulo :: Integer -> Integer -> Property
prop_mkFModulo n k =
  mkF @17 (n + 17 * k) === mkF @17 n

prop_addAssociative :: F17 -> F17 -> F17 -> Property
prop_addAssociative x y z =
  add x (add y z) === add (add x y) z

prop_addCommutative :: F17 -> F17 -> Property
prop_addCommutative x y =
  add x y === add y x

prop_addIdentity :: F17 -> Property
prop_addIdentity x =
  add x zero === x

prop_addInverse :: F17 -> Property
prop_addInverse x =
  add x (neg x) === zero

prop_subAsAddNeg :: F17 -> F17 -> Property
prop_subAsAddNeg x y =
  sub x y === add x (neg y)

prop_mulAssociative :: F17 -> F17 -> F17 -> Property
prop_mulAssociative x y z =
  mul x (mul y z) === mul (mul x y) z

prop_mulCommutative :: F17 -> F17 -> Property
prop_mulCommutative x y =
  mul x y === mul y x

prop_mulIdentity :: F17 -> Property
prop_mulIdentity x =
  mul x one === x

prop_mulInverse :: F17 -> Property
prop_mulInverse x =
  not (isZero x) ==>
    mul x (inv x) === one

prop_distributive :: F17 -> F17 -> F17 -> Property
prop_distributive x y z =
  mul x (add y z) === add (mul x y) (mul x z)

prop_divAsMulInv :: F17 -> F17 -> Property
prop_divAsMulInv x y =
  not (isZero y) ==>
    divF x y === mul x (inv y)

prop_powZero :: F17 -> Property
prop_powZero x =
  pow x 0 === one

prop_powOne :: F17 -> Property
prop_powOne x =
  pow x 1 === x

prop_powAdd :: F17 -> NonNegative Int -> NonNegative Int -> Property
prop_powAdd x (NonNegative m) (NonNegative n) =
  pow x (fromIntegral (m + n)) === mul (pow x (fromIntegral m)) (pow x (fromIntegral n))

prop_fermat :: F17 -> Property
prop_fermat x =
  not (isZero x) ==>
    pow x 16 === one
