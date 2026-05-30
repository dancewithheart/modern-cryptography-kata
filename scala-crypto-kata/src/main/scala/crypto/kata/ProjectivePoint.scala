package crypto.kata

enum ProjectivePoint {
  case Infinity
  case Homogeneous(x: Field, y: Field, z: Field)

  def normalize: Point =
    this match {
      case ProjectivePoint.Infinity =>
        Point.Infinity

      case ProjectivePoint.Homogeneous(_, _, z) if z == Field.zero =>
        Point.Infinity

      case ProjectivePoint.Homogeneous(x, y, z) =>
        val zInv =
          z.inverseNonZero

        Point.Affine(
          x = x * zInv,
          y = y * zInv
        )
    }
}

object ProjectivePoint {
  def fromAffine(p: Point): ProjectivePoint =
    p match {
      case Point.Infinity =>
        ProjectivePoint.Infinity

      case Point.Affine(x, y) =>
        ProjectivePoint.Homogeneous(x, y, Field.one)
    }

  def scale(p: Point, z: Field): ProjectivePoint = {
    require(z != Field.zero, "scale requires non-zero z")

    p match {
      case Point.Infinity =>
        ProjectivePoint.Infinity

      case Point.Affine(x, y) =>
        // Homogeneous representation:
        //
        //   affine x = X / Z
        //   affine y = Y / Z
        //
        // So choose:
        //
        //   X = x * Z
        //   Y = y * Z
        ProjectivePoint.Homogeneous(
          x = x * z,
          y = y * z,
          z = z
        )
    }
  }

  def normalizeAll(points: Vector[ProjectivePoint]): Vector[Point] = {
    val zValues =
      points.collect {
        case ProjectivePoint.Homogeneous(_, _, z) if z != Field.zero =>
          z
      }

    val zInverses =
      Field.batchInverseNonZero(zValues)

    val inverseByIndex =
      zInverses.iterator

    points.map {
      case ProjectivePoint.Infinity =>
        Point.Infinity

      case ProjectivePoint.Homogeneous(_, _, z) if z == Field.zero =>
        Point.Infinity

      case ProjectivePoint.Homogeneous(x, y, _) =>
        val zInv =
          inverseByIndex.next()

        Point.Affine(
          x = x * zInv,
          y = y * zInv
        )
    }
  }
}
