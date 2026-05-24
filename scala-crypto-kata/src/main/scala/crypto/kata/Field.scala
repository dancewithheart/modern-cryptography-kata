package crypto.kata

import scala.annotation.tailrec

enum FieldError {
  case DivisionByZero
}

final case class Field private(value: BigInt) {
  import Field.*

  def +(that: Field): Field = Field(this.value + that.value)

  def -(that: Field): Field = Field(this.value - that.value)

  def unary_- : Field = Field(-this.value)

  def *(that: Field): Field = Field(this.value * that.value)

  def squared: Field = this * this

  def cubed: Field = this * this * this

  def inverse: FieldError | Field = {
    if (this == Field.zero) FieldError.DivisionByZero
    else Field(modInverse(this.value, modulus))
  }

  def inverseNonZero: Field =
    Field(modInverse(this.value, modulus))

  def /(that: Field): Field = this * that.inverseNonZero
}

object Field {
  // Small toy prime. Good for exhaustive/property tests.
  // Not cryptographically meaningful.
  val modulus: BigInt = 97

  val zero: Field = Field(0)
  val one: Field  = Field(1)

  def apply(n: BigInt): Field = {
    // Modular normalization
    // Field(-1) == Field(96)
    // Field(98) == Field(1)
    val r = n.mod(modulus)
    new Field(r)
  }

  def elements: Vector[Field] =
    (BigInt(0) until modulus).map(Field(_)).toVector

  // Multiplicative inverse
  // inv 2 mod 97 = 49 because 2 * 49 = 98 ≡ 1 mod 97
  def modInverse(a: BigInt, m: BigInt): BigInt = {
      val bezout = extendedEuclidean(a.mod(m), m)
      if (bezout.gcd != 1) { // modular inverse only exists when gcd(a, m) = 1
        throw new ArithmeticException(s"$a has no inverse modulo $m")
      } else {
        bezout.x.mod(m)
      }
    }
}

// Bézout coefficient satisfy Bézout's identity: ax + by = gcd(a,b)
final case class Bezout(gcd: BigInt, x: BigInt, y: BigInt)

// EEA https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
def extendedEuclidean(a: BigInt, b: BigInt): Bezout = {
  @tailrec
  def loop(oldR: BigInt, r: BigInt, oldS: BigInt, s: BigInt, oldT: BigInt, t: BigInt): Bezout = {
    if (r == 0) Bezout(oldR, oldS, oldT)
    else {
      val q = oldR / r
      loop(oldR = r, r = oldR - q * r, oldS = s, s = oldS - q * s, oldT = t, t = oldT - q * t)
    }
  }

  loop(oldR = a, r = b, oldS = 1, s = 0, oldT = 0, t = 1)
}
