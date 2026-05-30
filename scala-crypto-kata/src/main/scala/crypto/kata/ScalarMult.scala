package crypto.kata

object ScalarMult {
  final case class PowerOfTwoMultiple(power: Int, point: Point)

  // Double-and-add scalar multiplication
  // kP = P + P + ... + P
  // for efficiently use binary decomposition of k
  // - powers of two multiples of P
  // - add selected powers
  // egzample:
  // 13P = 8P + 4P + P because 13 = 1101₂
  def multiply(k: BigInt, p: Point): Point = {
    require(k >= 0, s"scalar must be non-negative, got $k")

    binaryDecomposition(k, p).foldLeft(Point.Infinity) { case (acc, term) =>
      acc + term.point
    }
  }

  def binaryDecomposition(k: BigInt, p: Point): Vector[PowerOfTwoMultiple] = {
    require(k >= 0, s"scalar must be non-negative, got $k")

    var n = k
    var power = 0
    var base = p
    val selected = Vector.newBuilder[PowerOfTwoMultiple]

    while (n > 0) {
      if ((n & 1) == 1) {
        selected += PowerOfTwoMultiple(power, base)
      }

      base = base.double
      n = n >> 1
      power = power + 1
    }

    selected.result()
  }

  // sligly more optimized multiply - no extra allocations
  def multiplyDirect(k: BigInt, p: Point): Point = {
    require(k >= 0, s"scalar must be non-negative, got $k")

    var n = k
    var acc = Point.Infinity
    var base = p

    while (n > 0) {
      if ((n & 1) == 1) {
        acc = acc + base
      }

      base = base.double
      n = n >> 1
    }

    acc
  }

  def multiplySlow(k: BigInt, p: Point): Point = {
    require(k >= 0, s"scalar must be non-negative, got $k")

    var acc = Point.Infinity
    var i = BigInt(0)

    while (i < k) {
      acc = acc + p
      i = i + 1
    }

    acc
  }
}
