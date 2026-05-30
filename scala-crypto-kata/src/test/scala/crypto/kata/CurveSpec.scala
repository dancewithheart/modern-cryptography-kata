package crypto.kata

import zio.test.*

object CurveSpec extends ZIOSpecDefault {
  private val allPoints: Vector[Point] =
    Curve.points

  private val genPoint: Gen[Any, Point] =
    Gen.fromIterable(allPoints)

  override def spec: Spec[TestEnvironment, Any] =
    suite("Short Weierstrass elliptic curve over F_97")(
      test("test curve has points") {
        assertTrue(allPoints.nonEmpty)
      },

      test("generated affine points are on curve y² = x³ + ax + b") {
        check(genPoint) { p =>
          assertTrue(Curve.isOnCurve(p))
        }
      },

      // Checking is point is on curve is important due to invalid curve attacks
      // https://crypto.stackexchange.com/questions/71065/invalid-curve-attack-finding-low-order-points
      // https://safecurves.cr.yp.to/twist.html
      test("rejects affine points not on the curve") {
        val invalid =
          Point.Affine(Field(1), Field(1))

        assertTrue(!Curve.isOnCurve(invalid))
      },

      test("point addition preserves curve membership") {
        check(genPoint, genPoint) { (p, q) =>
          assertTrue(Curve.isOnCurve(p + q))
        }
      },

      test("infinity is identity") {
        check(genPoint) { p =>
          assertTrue(
            Point.Infinity + p == p,
            p + Point.Infinity == p
          )
        }
      },

      test("negation cancels: P + (-P) = O") {
        check(genPoint) { p =>
          assertTrue(p + (-p) == Point.Infinity)
        }
      },

      test("addition is commutative: P + Q = Q + P") {
        check(genPoint, genPoint) { (p, q) =>
          assertTrue(p + q == q + p)
        }
      },

      test("addition is associative: (P + Q) + R = P + (Q + R)") {
        check(genPoint, genPoint, genPoint) { (p, q, r) =>
          assertTrue((p + q) + r == p + (q + r))
        }
      },

      test("doubling agrees with addition to self") {
        check(genPoint) { p =>
          assertTrue(p.double == p + p)
        }
      },

      test("doubling point with y = 0 gives infinity") {
        val pointsWithVerticalTangent =
          allPoints.collect {
            case p @ Point.Affine(_, y) if y == Field.zero => p
          }

        assertTrue(pointsWithVerticalTangent.forall(p => p.double == Point.Infinity))
      }
    )
}
