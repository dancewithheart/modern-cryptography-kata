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

  // batch inverse
  // - invert one product
  // - recover all individual inverses by prefix/suffix products
  // instead of n inversions do 1 inversion + O(n) multiplications
  def batchInverse(values: Vector[Field]): Vector[Option[Field]] = {
    val nonZero =
      values.zipWithIndex.collect {
        case (x, index) if x != Field.zero => (x, index)
      }

    if (nonZero.isEmpty) {
      Vector.fill(values.length)(None)
    } else {
      val nonZeroValues =
        nonZero.map(_._1)

      val inverted =
        batchInverseNonZero(nonZeroValues)

      val result =
        Array.fill[Option[Field]](values.length)(None)

      nonZero.zip(inverted).foreach { case ((_, index), inverse) =>
        result(index) = Some(inverse)
      }

      result.toVector
    }
  }

  def batchInverseNonZero(values: Vector[Field]): Vector[Field] = {
    require(
      values.forall(_ != Field.zero),
      "batchInverseNonZero requires all inputs to be non-zero"
    )

    if (values.isEmpty) {
      Vector.empty
    } else {
      // prefixProducts(i) = values(0) * values(1) * ... * values(i)
      val prefixProducts =
        new Array[Field](values.length)

      var product =
        Field.one

      var i =
        0

      while (i < values.length) {
        product = product * values(i)
        prefixProducts(i) = product
        i = i + 1
      }

      // One expensive inversion.
      //
      // If product = a0 * a1 * ... * an,
      // then product.inverse gives:
      //
      //   1 / (a0 * a1 * ... * an)
      var inverseOfSuffix =
        product.inverseNonZero

      val result =
        new Array[Field](values.length)

      i = values.length - 1

      while (i >= 0) {
        val productBeforeCurrent =
          if (i == 0) Field.one
          else prefixProducts(i - 1)

        // inverseOfSuffix currently contains:
        //
        //   1 / (values(0) * ... * values(i))
        //
        // Multiplying by the product before values(i) cancels
        // everything except values(i):
        //
        //   productBeforeCurrent / (productBeforeCurrent * values(i))
        // = 1 / values(i)
        result(i) =
          inverseOfSuffix * productBeforeCurrent

        // Move one step left:
        //
        //   1 / (values(0) * ... * values(i))
        //     * values(i)
        // = 1 / (values(0) * ... * values(i - 1))
        inverseOfSuffix =
          inverseOfSuffix * values(i)

        i = i - 1
      }

      result.toVector
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
