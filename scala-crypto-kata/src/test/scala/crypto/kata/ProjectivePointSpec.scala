package crypto.kata

import zio.test.*

object ProjectivePointSpec extends ZIOSpecDefault {
  private val genAffinePoint: Gen[Any, Point] =
    Gen.fromIterable(Curve.points.collect {
      case p @ Point.Affine(_, _) => p
    })

  private val genNonZeroField: Gen[Any, Field] =
    Gen.bigInt(BigInt(-1000), BigInt(1000))
      .map(Field(_))
      .filter(_ != Field.zero)

  override def spec: Spec[TestEnvironment, Any] =
    suite("Projective point affine normalization")(
      test("normalizing an affine point embedded with z = 1 gives the same point") {
        check(genAffinePoint) { p =>
          assertTrue(
            ProjectivePoint.fromAffine(p).normalize == p
          )
        }
      },

      test("normalizing a scaled homogeneous point gives the original affine point") {
        check(genAffinePoint, genNonZeroField) { (p, z) =>
          assertTrue(
            ProjectivePoint.scale(p, z).normalize == p
          )
        }
      },

      test("batch normalization agrees with individual normalization") {
        val genProjectivePoints =
          Gen.chunkOfBounded(0, 32) {
            for {
              p <- genAffinePoint
              z <- genNonZeroField
            } yield ProjectivePoint.scale(p, z)
          }.map(_.toVector)

        check(genProjectivePoints) { points =>
          assertTrue(
            ProjectivePoint.normalizeAll(points) ==
              points.map(_.normalize)
          )
        }
      },

      test("batch normalization preserves curve membership") {
        val genProjectivePoints =
          Gen.chunkOfBounded(0, 32) {
            for {
              p <- genAffinePoint
              z <- genNonZeroField
            } yield ProjectivePoint.scale(p, z)
          }.map(_.toVector)

        check(genProjectivePoints) { points =>
          val normalized =
            ProjectivePoint.normalizeAll(points)

          assertTrue(
            normalized.forall(Curve.isOnCurve)
          )
        }
      }
    )
}
