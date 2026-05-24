package crypto.kata

enum Point {
  // point at infinity O
  // is the neutral element of the elliptic-curve group
  // behaves like zero for point addition:
  //  O + P = P
  //  P + O = P
  case Infinity
  case Affine(x: Field, y: Field)

  def unary_- : Point = this match {
    case Point.Infinity     => Point.Infinity
    case Point.Affine(x, y) => Point.Affine(x, -y)
  }

  def +(that: Point): Point = Curve.add(this, that)

  def double: Point = this + this
}

object Curve {
  import Point.*

  // Toy curve: y^2 = x^3 + ax + b over F_97
  val a: Field = Field(2)
  val b: Field = Field(3)

  def isOnCurve(p: Point): Boolean =
    p match {
      case Infinity => true
      case Affine(x, y) => y.squared == x.cubed + a * x + b
    }

  // Elliptic-curve point addition
  def add(p: Point, q: Point): Point =
    (p, q) match {
      // Exceptional cases in point addition
      case (Infinity, _) => q // O + P
      case (_, Infinity) => p // P + O

      // Same x-coordinate and opposite y-coordinate means the line through
      // the two points is vertical. A vertical line intersects the curve at O
      case (Affine(x1, y1), Affine(x2, y2)) if x1 == x2 && y1 == -y2 =>
        Infinity

      // doubling
      case (Affine(x1, y1), Affine(x2, y2)) if x1 == x2 && y1 == y2 =>
        // Tangent slope: λ = (3*x1^2 + a) / 2*y1
        val denominator = Field(2) * y1

        denominator.inverse match {
          // If y1 = 0, the tangent is vertical and the result is infinity.
          case FieldError.DivisionByZero => Infinity
          case inv : Field =>
            val lambda = (Field(3) * x1.squared + a) * inv
            // compute the resulting point
            val x3 = lambda.squared - x1 - x1
            val y3 = lambda * (x1 - x3) - y1
            Affine(x3, y3)
        }

      // For two different points, you compute the chord slope
      case (Affine(x1, y1), Affine(x2, y2)) =>
        // Chord slope:  λ = (y2 - y1) / (x2 - x1)
        // The vertical-line case x1 = x2 was handled above
        val lambda = (y2 - y1) / (x2 - x1)
        val x3 = lambda.squared - x1 - x2
        val y3 = lambda * (x1 - x3) - y1
        Affine(x3, y3)
    }

  def points: Vector[Point] = {
    val affine =
      for {
        x <- Field.elements
        y <- Field.elements
        p = Affine(x, y)
        if isOnCurve(p)
      } yield p

    Infinity +: affine
  }
}
