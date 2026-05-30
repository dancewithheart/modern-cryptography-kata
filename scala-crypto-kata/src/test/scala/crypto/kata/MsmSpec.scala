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
      },

      test("bucketed MSM handles zero scalars") {
        check(genPoint, Gen.int(1, 8)) { (p, window) =>
          val terms = List(Msm.Term(0, p))
          assertTrue(Msm.bucketed(terms, window) == Point.Infinity)
        }
      },

      test("bucketed MSM handles infinity points") {
        check(genScalar, Gen.int(1, 8)) { (k, window) =>
          val terms = List(Msm.Term(k, Point.Infinity))
          assertTrue(Msm.bucketed(terms, window) == Point.Infinity)
        }
      },

      test("bucketed MSM handles cancellation inside buckets") {
        check(genScalar, genPoint, Gen.int(1, 8)) { (k, p, window) =>
          val terms =
            List(
              Msm.Term(k, p),
              Msm.Term(k, -p)
            )

          assertTrue(Msm.bucketed(terms, window) == Point.Infinity)
        }
      },

      test("bucketed MSM handles duplicate bases") {
        check(genScalar, genScalar, genPoint, Gen.int(1, 8)) { (m, n, p, window) =>
          val terms =
            List(
              Msm.Term(m, p),
              Msm.Term(n, p)
            )

          assertTrue(
            Msm.bucketed(terms, window) ==
              ScalarMult.multiply(m + n, p)
          )
        }
      },

      test("bucket cancellation does not disturb other buckets") {
        check(Gen.int(1, 7), Gen.int(1, 7), genPoint, genPoint) {
          (cancelDigit, liveDigit, p, q) =>
            val terms =
              List(
                Msm.Term(BigInt(cancelDigit), p),
                Msm.Term(BigInt(cancelDigit), -p),
                Msm.Term(BigInt(liveDigit), q)
              )

            assertTrue(
              Msm.bucketed(terms, window = 3) ==
                ScalarMult.multiply(BigInt(liveDigit), q)
            )
        }
      },

      test("weighted bucket sum agrees with direct bucket weighting") {
        val genBuckets = Gen.chunkOfBounded(2, 16)(genPoint).map(_.toArray)

        check(genBuckets) { buckets =>
          val direct = buckets.zipWithIndex.foldLeft(Point.Infinity) {
            case (acc, (bucket, digit)) =>
              acc + ScalarMult.multiply(BigInt(digit), bucket)
          }
          val optimized = Msm.weightedBucketSum(buckets)

          assertTrue(optimized == direct)
        }
      },

      test("optimized bucket summation agrees with direct bucket weighting") {
        check(genTerms, Gen.int(1, 8)) { (terms, window) =>
          assertTrue(
            Msm.bucketed(terms, window) == Msm.bucketedDirect(terms, window)
          )
        }
      }
    )
}