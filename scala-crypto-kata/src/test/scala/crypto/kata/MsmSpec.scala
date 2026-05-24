package crypto.kata

import zio.Chunk
import zio.test.*

object MsmSpec extends ZIOSpecDefault {
  private val genPoint: Gen[Any, Point] =
    Gen.fromIterable(Curve.points)

  private val genScalar: Gen[Any, BigInt] =
    Gen.bigInt(BigInt(0), BigInt(100))

  private val genTerm: Gen[Any, Msm.Term] =
    for {
      k <- genScalar
      p <- genPoint
    } yield Msm.Term(k, p)

  private val genTerms: Gen[Any, Chunk[Msm.Term]] =
    Gen.chunkOfBounded(0, 20)(genTerm)

  override def spec: Spec[TestEnvironment, Any] =
    suite("Multi-scalar multiplication (MSM)")(
      test("empty MSM is infinity") {
        assertTrue(Msm.naive(Nil) == Point.Infinity)
      },

      test("single-term MSM agrees with scalar multiplication") {
        check(genScalar, genPoint) { (k, p) =>
          assertTrue(
            Msm.naive(List(Msm.Term(k, p))) == ScalarMult.multiply(k, p)
          )
        }
      },

      test("naive MSM agrees with very slow MSM") {
        check(genTerms) { terms =>
          assertTrue(Msm.naive(terms) == Msm.naiveSlow(terms))
        }
      },

      test("bucketed MSM agrees with naive MSM") {
        check(genTerms, Gen.int(1, 8)) { (terms, window) =>
          assertTrue(Msm.bucketed(terms, window) == Msm.naive(terms))
        }
      },

      test("MSM is additive over concatenation") {
        check(genTerms, genTerms) { (xs, ys) =>
          assertTrue(
            Msm.naive(xs ++ ys) == (Msm.naive(xs) + Msm.naive(ys))
          )
        }
      }
    )
}
