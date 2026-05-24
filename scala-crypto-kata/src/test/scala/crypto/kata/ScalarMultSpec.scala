package crypto.kata

import zio.test.*

object ScalarMultSpec extends ZIOSpecDefault {
  private val genPoint: Gen[Any, Point] =
    Gen.fromIterable(Curve.points)

  private val genSmallScalar: Gen[Any, BigInt] =
    Gen.bigInt(BigInt(0), BigInt(200))

  override def spec: Spec[TestEnvironment, Any] =
    suite("Double-and-add scalar multiplication")(
      test("binary decomposition of 13P selects P, 4P, and 8P") {
        check(genPoint) { p =>
          val terms =
            ScalarMult.binaryDecomposition(13, p)

          assertTrue(
            terms.map(_.power) == Vector(0, 2, 3),
            terms.map(_.point) == Vector(
              p,
              ScalarMult.multiplySlow(4, p),
              ScalarMult.multiplySlow(8, p)
            )
          )
        }
      },

      test("binary decomposition reconstructs scalar multiplication") {
        check(genSmallScalar, genPoint) { (k, p) =>
          val reconstructed =
            ScalarMult.binaryDecomposition(k, p).foldLeft(Point.Infinity) {
              case (acc, term) => acc + term.point
            }

          assertTrue(
            reconstructed == ScalarMult.multiplySlow(k, p)
          )
        }
      },

      test("0 * p = infinity") {
        check(genPoint) { p =>
          assertTrue(ScalarMult.multiply(0, p) == Point.Infinity)
        }
      },

      test("1 * p = p") {
        check(genPoint) { p =>
          assertTrue(ScalarMult.multiply(1, p) == p)
        }
      },

      test("2 * p = p + p") {
        check(genPoint) { p =>
          assertTrue(ScalarMult.multiply(2, p) == p + p)
        }
      },

      test("fast scalar multiplication using double-and-add equals slow repeated addition") {
        check(genSmallScalar, genPoint) { (k, p) =>
          assertTrue(
            ScalarMult.multiply(k, p) == ScalarMult.multiplySlow(k, p)
          )
        }
      },

      test("direct double-and-add agrees with decomposition version") {
        check(genSmallScalar, genPoint) { (k, p) =>
          assertTrue(
            ScalarMult.multiplyDirect(k, p) == ScalarMult.multiply(k, p)
          )
        }
      },

      test("(m + n) * p = m*p + n*p") {
        check(genSmallScalar, genSmallScalar, genPoint) { (m, n, p) =>
          assertTrue(
            ScalarMult.multiply(m + n, p) ==
              (ScalarMult.multiply(m, p) + ScalarMult.multiply(n, p))
          )
        }
      },

      test("m * (p + q) = m*p + m*q") {
        check(genSmallScalar, genPoint, genPoint) { (m, p, q) =>
          assertTrue(
            ScalarMult.multiply(m, p + q) ==
              (ScalarMult.multiply(m, p) + ScalarMult.multiply(m, q))
          )
        }
      }
    )
}
