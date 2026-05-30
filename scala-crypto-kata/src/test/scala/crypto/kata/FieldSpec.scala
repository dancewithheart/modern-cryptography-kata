package crypto.kata

import zio.test.*

object FieldSpec extends ZIOSpecDefault {
  private val genF: Gen[Any, Field] =
    Gen.bigInt(BigInt(-1000), BigInt(1000)).map(Field(_))

  override def spec: Spec[TestEnvironment, Any] =
    suite("Prime field F_97")(
      test("addition is associative") {
        check(genF, genF, genF) { (x, y, z) =>
          assertTrue((x + y) + z == x + (y + z))
        }
      },

      test("addition is commutative") {
        check(genF, genF) { (x, y) =>
          assertTrue(x + y == y + x)
        }
      },

      test("zero is additive identity") {
        check(genF) { x =>
          assertTrue(
            x + Field.zero == x,
            Field.zero + x == x
          )
        }
      },

      test("additive inverse cancels") {
        check(genF) { x =>
          assertTrue(x + (-x) == Field.zero)
        }
      },

      test("multiplication is associative") {
        check(genF, genF, genF) { (x, y, z) =>
          assertTrue((x * y) * z == x * (y * z))
        }
      },

      test("multiplication is commutative") {
        check(genF, genF) { (x, y) =>
          assertTrue(x * y == y * x)
        }
      },

      test("one is multiplicative identity") {
        check(genF) { x =>
          assertTrue(
            x * Field.one == x,
            Field.one * x == x
          )
        }
      },

      test("non-zero elements have multiplicative inverse") {
        check(genF.filter(_ != Field.zero)) { x =>
          assertTrue(x * x.inverseNonZero == Field.one)
        }
      },

      test("distributivity") {
        check(genF, genF, genF) { (x, y, z) =>
          assertTrue(x * (y + z) == x * y + x * z)
        }
      },

      test("modInverse satisfies a * a⁻¹ = 1 mod p") {
        check(Gen.bigInt(1, Field.modulus - 1)) { a =>
          val inv = Field.modInverse(a, Field.modulus)
          assertTrue((a * inv).mod(Field.modulus) == 1)
        }
      },

      test("modInverse agrees extended Euclidean modular inverse") {
        check(Gen.bigInt(1, Field.modulus - 1)) { a =>
          val inv = Field.modInverse(a, Field.modulus)
          val extracted = extendedEuclidean(a, Field.modulus)

          assertTrue(
            extracted.gcd == 1,
            inv == extracted.x.mod(Field.modulus)
          )
        }
      },

      // modular inverse only exists when gcd(a, m) = 1
      // for numbers that are not coprime it should fail
      test("modular inverse rejects non-coprime input") {
        import scala.util.Try
        val failure = Try(Field.modInverse(6, 9)).isFailure

        assertTrue(failure)
      },

      test("batchInverseNonZero agrees with individual inverses") {
        val genNonZeroFields = Gen.chunkOfBounded(0, 32)(genF.filter(_ != Field.zero)).map(_.toVector)

        check(genNonZeroFields) { values =>
          val expected = values.map(_.inverseNonZero)
          val actual = Field.batchInverseNonZero(values)

          assertTrue(actual == expected)
        }
      },

      test("batchInverse handles zeros safely") {
        val genFields = Gen.chunkOfBounded(0, 32)(genF).map(_.toVector)
        check(genFields) { values =>
          val expected =
            values.map {
              case Field.zero => None
              case x          => Some(x.inverseNonZero)
            }
          val actual = Field.batchInverse(values)

          assertTrue(actual == expected)
        }
      },

      test("batchInverseNonZero outputs multiplicative inverses") {
        val genNonZeroFields = Gen.chunkOfBounded(0, 32)(genF.filter(_ != Field.zero)).map(_.toVector)
        check(genNonZeroFields) { values =>
          val inverses = Field.batchInverseNonZero(values)

          assertTrue(
            values.zip(inverses).forall { case (x, inv) =>
              x * inv == Field.one
            }
          )
        }
      }
    )
}
