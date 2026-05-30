{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

module Crypto.Kata.Field
  ( F
  , mkF
  , unF
  , zero
  , one
  , add
  , neg
  , sub
  , mul
  , pow
  , inv
  , divF
  , modulus
  ) where

import GHC.TypeNats
  ( KnownNat
  , Nat
  , natVal
  )
import Data.Proxy
  ( Proxy (Proxy)
  )
import Numeric.Natural
  ( Natural
  )

-- | Element of the prime finite field F_p.
--
-- Invariant:
--
--   0 <= unF x < p
--
-- For Kata 1 we test with p = 17.
-- A real library would encode or check primality more carefully.
newtype F (p :: Nat) = F Integer
  deriving stock (Eq, Ord)

instance Show (F p) where
  show (F x) = show x

-- | Return the canonical representative of a field element.
unF :: F p -> Integer
unF (F x) = x

-- | Runtime value of the type-level modulus.
modulus :: forall p proxy. KnownNat p => proxy p -> Integer
modulus _ = toInteger (natVal (Proxy @p))

-- | Smart constructor.
--
-- Required invariant:
--
--   0 <= unF (mkF x) < p
--
-- Examples over F_17:
--
--   mkF 20 == mkF 3
--   mkF (-1) == mkF 16
mkF :: forall p. KnownNat p => Integer -> F p
mkF = todo "mkF"

zero :: KnownNat p => F p
zero = todo "zero"

one :: KnownNat p => F p
one = todo "one"

add :: KnownNat p => F p -> F p -> F p
add = todo "add"

neg :: KnownNat p => F p -> F p
neg = todo "neg"

sub :: KnownNat p => F p -> F p -> F p
sub = todo "sub"

mul :: KnownNat p => F p -> F p -> F p
mul = todo "mul"

-- | Exponentiation by repeated squaring.
--
-- For this kata, negative exponents are intentionally out of scope.
pow :: KnownNat p => F p -> Natural -> F p
pow = todo "pow"

-- | Multiplicative inverse.
--
-- For non-zero x:
--
--   mul x (inv x) == one
--
-- For zero, this function should fail explicitly.
inv :: KnownNat p => F p -> F p
inv = todo "inv"

divF :: KnownNat p => F p -> F p -> F p
divF = todo "divF"

todo :: String -> a
todo name = error ("TODO(Kata 1): implement " <> name)
