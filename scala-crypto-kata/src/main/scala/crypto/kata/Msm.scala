package crypto.kata

object Msm {
  final case class Term(scalar: BigInt, point: Point)

  def naive(terms: Iterable[Term]): Point =
    terms.foldLeft(Point.Infinity) { case (acc, Term(k, p)) =>
      acc + ScalarMult.multiply(k, p)
    }

  def naiveSlow(terms: Iterable[Term]): Point =
    terms.foldLeft(Point.Infinity) { case (acc, Term(k, p)) =>
      acc + ScalarMult.multiplySlow(k, p)
    }

  // Stub for later.
  //
  // First property target:
  //
  //   bucketed(terms, window) == naive(terms)
  //
  def bucketed(terms: Iterable[Term], window: Int): Point = {
    require(window > 0, s"window must be positive, got $window")

    // Deliberately boring placeholder.
    // Replace this with bucket/Pippenger implementation later.
    naive(terms)
  }
}
