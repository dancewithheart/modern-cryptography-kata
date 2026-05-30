module Crypto.Algebra.F17 where

open import Agda.Builtin.Nat using (Nat; zero; suc; _+_; _*_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Primitive using (lsuc; Level)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; cong)
open import Data.Empty using (⊥-elim)

------------------------------------------------------------------------
-- Field elements: F₁₇

data F17 : Set where
  f0  : F17
  f1  : F17
  f2  : F17
  f3  : F17
  f4  : F17
  f5  : F17
  f6  : F17
  f7  : F17
  f8  : F17
  f9  : F17
  f10 : F17
  f11 : F17
  f12 : F17
  f13 : F17
  f14 : F17
  f15 : F17
  f16 : F17

------------------------------------------------------------------------
-- Conversion to/from Nat modulo 17

toNat : F17 → Nat
toNat f0  = 0
toNat f1  = 1
toNat f2  = 2
toNat f3  = 3
toNat f4  = 4
toNat f5  = 5
toNat f6  = 6
toNat f7  = 7
toNat f8  = 8
toNat f9  = 9
toNat f10 = 10
toNat f11 = 11
toNat f12 = 12
toNat f13 = 13
toNat f14 = 14
toNat f15 = 15
toNat f16 = 16

next : F17 → F17
next f0  = f1
next f1  = f2
next f2  = f3
next f3  = f4
next f4  = f5
next f5  = f6
next f6  = f7
next f7  = f8
next f8  = f9
next f9  = f10
next f10 = f11
next f11 = f12
next f12 = f13
next f13 = f14
next f14 = f15
next f15 = f16
next f16 = f0

fromNat : Nat → F17
fromNat zero    = f0
fromNat (suc n) = next (fromNat n)

------------------------------------------------------------------------
-- Ring operations

addN : F17 → Nat → F17
addN x zero    = x
addN x (suc n) = next (addN x n)

_+F_ : F17 → F17 → F17
x +F y = addN x (toNat y)
-- x +F y = fromNat (toNat x + toNat y)

_*F_ : F17 → F17 → F17
x *F y = fromNat (toNat x * toNat y)

neg : F17 → F17
neg f0  = f0
neg f1  = f16
neg f2  = f15
neg f3  = f14
neg f4  = f13
neg f5  = f12
neg f6  = f11
neg f7  = f10
neg f8  = f9
neg f9  = f8
neg f10 = f7
neg f11 = f6
neg f12 = f5
neg f13 = f4
neg f14 = f3
neg f15 = f2
neg f16 = f1

_-F_ : F17 → F17 → F17
x -F y = x +F neg y

------------------------------------------------------------------------
-- Exponentiation

_^F_ : F17 → Nat → F17
x ^F zero    = f1
x ^F (suc n) = x *F (x ^F n)

------------------------------------------------------------------------
-- Multiplicative inverse.
--
-- For F₁₇, every nonzero element has inverse x¹⁵ by Fermat:
--
--   x¹⁶ = 1
--   therefore x⁻¹ = x¹⁵
--
-- We keep inv f0 = f0 as a total placeholder.
-- Later, refine this with Maybe / Dec / nonzero proof.

inv : (x : F17) → x ≢ f0 → F17
inv f0 p = ⊥-elim (p refl)
inv x p = x ^F 15

_÷F_ : F17 → (y : F17) → y ≢ f0 → F17
x ÷F y = \ p -> x *F (inv y p)
--x ÷F y p = x *F (inv y p)

infixl 7 _*F_ _÷F_
infixl 6 _+F_ _-F_
infixr 8 _^F_

------------------------------------------------------------------------
-- Small executable sanity checks

example-add : f9 +F f10 ≡ f2
example-add = refl

example-mul : f5 *F f7 ≡ f1
example-mul = refl

example-neg : f5 +F neg f5 ≡ f0
example-neg = refl

example-pow : f3 ^F 4 ≡ f13
example-pow = refl

f5≠f0 : f5 ≢ f0
f5≠f0 ()

example-inv : f5 *F inv f5 f5≠f0 ≡ f1
example-inv = refl

example-div : (f10 ÷F f5) f5≠f0 ≡ f2
example-div = refl

variable u : Level

record Group : Set (lsuc u) where
  field
    Carrier  : Set u
    Unit     : Carrier
    _+G_     : Carrier -> Carrier → Carrier
    -G_      : Carrier -> Carrier

    +G-right-unit : (x : Carrier) → x +G Unit ≡ x
    +G-left-unit  : (x : Carrier) → Unit +G x ≡ x
    +G-assoc      : (x y z : Carrier)
               → x +G (y +G z) ≡ (x +G y) +G z
    -G-left-inv   : (x : Carrier)
              → x +G (-G x) ≡ Unit
    -G-right-inv   : (x : Carrier)
              → (-G x) +G x ≡ Unit
  infixl 6 _+G_

+F-right-unit : ∀ (x : F17) → (x +F f0) ≡ x
+F-right-unit x = refl

addN-zero-id : ∀ (x : F17) → addN x zero ≡ x
addN-zero-id x = refl

addN-f0=fromNat : ∀ (n : Nat) → addN f0 n ≡ (fromNat n)
addN-f0=fromNat zero    = refl
addN-f0=fromNat (suc n) = cong next (addN-f0=fromNat n)

addN-f0-toNat : ∀ (x : F17) → addN f0 (toNat x) ≡ x
addN-f0-toNat f0 = refl
addN-f0-toNat f1 = refl
addN-f0-toNat f2 = refl
addN-f0-toNat f3 = refl
addN-f0-toNat f4 = refl
addN-f0-toNat f5 = refl
addN-f0-toNat f6 = refl
addN-f0-toNat f7 = refl
addN-f0-toNat f8 = refl
addN-f0-toNat f9 = refl
addN-f0-toNat f10 = refl
addN-f0-toNat f11 = refl
addN-f0-toNat f12 = refl
addN-f0-toNat f13 = refl
addN-f0-toNat f14 = refl
addN-f0-toNat f15 = refl
addN-f0-toNat f16 = refl

fromNat-toNat : ∀ (x : F17) → fromNat (toNat x) ≡ x
fromNat-toNat f0 = refl
fromNat-toNat f1 = refl
fromNat-toNat f2 = refl
fromNat-toNat f3 = refl
fromNat-toNat f4 = refl
fromNat-toNat f5 = refl
fromNat-toNat f6 = refl
fromNat-toNat f7 = refl
fromNat-toNat f8 = refl
fromNat-toNat f9 = refl
fromNat-toNat f10 = refl
fromNat-toNat f11 = refl
fromNat-toNat f12 = refl
fromNat-toNat f13 = refl
fromNat-toNat f14 = refl
fromNat-toNat f15 = refl
fromNat-toNat f16 = refl

+F-left-unit : ∀ (x : F17) → (f0 +F x) ≡ x
+F-left-unit x
  rewrite addN-f0=fromNat (toNat x) = fromNat-toNat x

+F-assoc : ∀ (x y z : F17) → x +F (y +F z) ≡ x +F y +F z
+F-assoc x y z = {!!}

neg-left-inv : (x : F17) → x +F neg x ≡ f0
neg-left-inv f0 = refl
neg-left-inv f1 = refl
neg-left-inv f2 = refl
neg-left-inv f3 = refl
neg-left-inv f4 = refl
neg-left-inv f5 = refl
neg-left-inv f6 = refl
neg-left-inv f7 = refl
neg-left-inv f8 = refl
neg-left-inv f9 = refl
neg-left-inv f10 = refl
neg-left-inv f11 = refl
neg-left-inv f12 = refl
neg-left-inv f13 = refl
neg-left-inv f14 = refl
neg-left-inv f15 = refl
neg-left-inv f16 = refl

neg-right-inv : (x : F17) → neg x +F x ≡ f0
neg-right-inv f0 = refl
neg-right-inv f1 = refl
neg-right-inv f2 = refl
neg-right-inv f3 = refl
neg-right-inv f4 = refl
neg-right-inv f5 = refl
neg-right-inv f6 = refl
neg-right-inv f7 = refl
neg-right-inv f8 = refl
neg-right-inv f9 = refl
neg-right-inv f10 = refl
neg-right-inv f11 = refl
neg-right-inv f12 = refl
neg-right-inv f13 = refl
neg-right-inv f14 = refl
neg-right-inv f15 = refl
neg-right-inv f16 = refl

F17_is_group : Group
F17_is_group = record
  { Carrier = F17
  ; Unit = f0
  ; _+G_ = _+F_
  ; -G_  = neg
  ; +G-right-unit = +F-right-unit
  ; +G-left-unit  = +F-left-unit
  ; +G-assoc      = +F-assoc
  ; -G-left-inv   = neg-left-inv
  ; -G-right-inv  = neg-right-inv
  }


-- unused
{-
foo4 : ∀ n → next (fromNat n) ≡ fromNat (suc n)
foo4 = {!!}
-}

{- this is wrong
toNat-next=suc-toNat : ∀ (x : F17) → toNat (next x) ≡ suc (toNat x)
toNat-next=suc-toNat f0 = refl
toNat-next=suc-toNat f1 = refl
toNat-next=suc-toNat f2 = refl
toNat-next=suc-toNat f3 = refl
toNat-next=suc-toNat f4 = refl
toNat-next=suc-toNat f5 = refl
toNat-next=suc-toNat f6 = refl
toNat-next=suc-toNat f7 = refl
toNat-next=suc-toNat f8 = refl
toNat-next=suc-toNat f9 = refl
toNat-next=suc-toNat f10 = refl
toNat-next=suc-toNat f11 = refl
toNat-next=suc-toNat f12 = refl
toNat-next=suc-toNat f13 = refl
toNat-next=suc-toNat f14 = refl
toNat-next=suc-toNat f15 = refl
toNat-next=suc-toNat f16 = {!refl!}

toNat-fromNat : ∀ (n : Nat) → toNat (fromNat n) ≡ n
toNat-fromNat zero    = refl
toNat-fromNat (suc n)
   rewrite toNat-next=suc-toNat (fromNat n)
   = cong suc (toNat-fromNat n)
-}
