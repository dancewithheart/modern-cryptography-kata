package crypto.kata

import zio.test.*

object ExtendedEuclideanSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment, Any] =
    suite("extended Euclidean algorithm ")(
      test("satisfies Bézout identity") {
        check(Gen.bigInt(1, 10000), Gen.bigInt(1, 10000)) { (a, b) =>
          val bezout = extendedEuclidean(a, b)
          assertTrue(a * bezout.x + b * bezout.y == bezout.gcd)
        }
      },

      test("algorithm computes gcd") {
        check(Gen.bigInt(1, 10000), Gen.bigInt(1, 10000)) { (a, b) =>
          val bezout = extendedEuclidean(a, b)
          assertTrue(bezout.gcd == a.gcd(b))
        }
      }
    )
}
