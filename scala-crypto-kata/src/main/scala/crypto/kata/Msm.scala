package crypto.kata

object Msm {
  final case class Term(scalar: BigInt, point: Point)

  // Naive multi-scalar multiplication
  // Σ kᵢPᵢ
  def naive(terms: Iterable[Term]): Point =
    terms.foldLeft(Point.Infinity) { case (acc, Term(k, p)) =>
      acc + ScalarMult.multiply(k, p)
    }

  def naiveSlow(terms: Iterable[Term]): Point =
    terms.foldLeft(Point.Infinity) { case (acc, Term(k, p)) =>
      acc + ScalarMult.multiplySlow(k, p)
    }

  def bucketed(terms: Iterable[Term], window: Int): Point = bucketedPippenger(terms, window)

  def bucketedPippenger(terms: Iterable[Term], window: Int): Point = {
    require(window > 0, s"window must be positive, got $window")

    // Remove terms that can never contribute:
    //   0 * P = O
    //   k * O = O
    val normalizedTerms = terms.toVector.filter { case Term(k, p) =>
        k != 0 && p != Point.Infinity }

    if (normalizedTerms.isEmpty) Point.Infinity
    else {
      val radix: BigInt = BigInt(1) << window

      require(
        radix <= Int.MaxValue,
        s"window too large for array-backed buckets: window=$window, radix=$radix"
      )

      val bucketCount: Int = radix.toInt
      val maxScalar: BigInt = normalizedTerms.map(_.scalar).max
      val windows: Int = scalarWindowCount(maxScalar, window)

      var result: Point = Point.Infinity
      var windowIndex = 0
      while (windowIndex < windows) {
        val buckets: Array[Point] = Array.fill(bucketCount)(Point.Infinity)

        // Bucket accumulation:
        //
        // For this window, group points by the scalar digit appearing
        // in that window.
        //
        // Example with window = 3:
        //   scalar digit 5 means:
        //     bucket[5] += point
        // Zero digit is skipped because:
        //   0 * P = O
        normalizedTerms.foreach { case Term(scalar, point) =>
          val digit = windowDigit(scalar, window, windowIndex).toInt

          if (digit != 0) {
            // Cancellation can happen here implicitly:
            //   bucket[d] = P
            //   point     = -P
            // then:
            //   bucket[d] + point = O
            buckets(digit) = buckets(digit) + point
          }
        }

        val windowSum = weightedBucketSum(buckets)
        val shift = BigInt(1) << (window * windowIndex)
        val shiftedWindowSum = ScalarMult.multiply(shift, windowSum)
        result = result + shiftedWindowSum
        windowIndex = windowIndex + 1
      }
      result
    }
  }

  // Computes:
  //   1*bucket[1] + 2*bucket[2] + ... + (n-1)*bucket[n-1]
  //
  // without calling scalar multiplication for every bucket.
  // Running-sum trick:
  //   running = O
  //   sum     = O
  //
  //   for digit from max down to 1:
  //     running += bucket[digit]
  //     sum     += running
  //
  // Example with buckets 1, 2, 3:
  //
  //   digit = 3:
  //     running = bucket[3]
  //     sum     = bucket[3]
  //
  //   digit = 2:
  //     running = bucket[3] + bucket[2]
  //     sum     = 2*bucket[3] + bucket[2]
  //
  //   digit = 1:
  //     running = bucket[3] + bucket[2] + bucket[1]
  //     sum     = 3*bucket[3] + 2*bucket[2] + bucket[1]
  private[kata] def weightedBucketSum(buckets: Array[Point]): Point = {
    var running: Point = Point.Infinity
    var sum: Point = Point.Infinity
    var digit = buckets.length - 1

    while (digit >= 1) {
      running = running + buckets(digit)
      sum = sum + running
      digit = digit - 1
    }

    sum
  }

  // Bucketed MSM
  // reuse work by grouping points that have the same scalar digit
  //   bucketed(terms, window) == naive(terms)
  // example:
  // window = 2
  // radix = 4
  // terms = 6P + 3Q
  // decomposed scalars in base 4
  // 6 = 2 + 1*4
  // 3 = 3 + 0*4
  // window 0:
  //  6P has digit 2 -> bucket[2] += P
  //  3Q has digit 3 -> bucket[3] += Q
  // windowSum = 2P + 3Q
  // shift = 1
  // contribution = 2P + 3Q
  // window 1
  // 6P has digit 1 -> bucket[1] += P
  // 3Q has digit 0 -> skipped
  // result: (2P + 3Q) + 4P = 6P + 3Q
  def bucketedDirect(terms: Iterable[Term], window: Int): Point = {
    require(window > 0, s"window must be positive, got $window")

    // Remove terms that can never contribute:
    //   0 * P = O
    //   k * O = O
    val normalizedTerms = terms.toVector.filter {
        case Term(k, p) =>
          k != 0 && p != Point.Infinity
      }

    if (normalizedTerms.isEmpty) Point.Infinity
    else {
      val radix: BigInt = BigInt(1) << window
      val maxScalar: BigInt = normalizedTerms.map(_.scalar).max
      val windows: Int = scalarWindowCount(maxScalar, window)

      var result: Point = Point.Infinity
      var windowIndex = 0
      while (windowIndex < windows) {
        // Bucket accumulation
        // 1. create bucket for every window
        val buckets: Array[Point] = Array.fill(radix.toInt)(Point.Infinity)
        // 2. for each term
        // - Look at this scalar's digit in this window.
        // - Put its point into the matching bucket.
        normalizedTerms.foreach { case Term(scalar, point) =>
          val digit = windowDigit(scalar, window, windowIndex).toInt

          // Zero digit skipping
          // e.g. 64 = digits [0,0,1] with window = 3 skip first and second index
          if (digit != 0) {
            // cancellation inside buckets
            // happens if buckets(digit) = -P and point = P handled in Curve addition
            buckets(digit) = buckets(digit) + point
          }
        }

        var windowSum: Point = Point.Infinity
        var digit = 1
        while (digit < radix.toInt) {
          // e.g. for bucket[3] = P + R
          // contribution is 3 * (P + R) = 3P + 3R
          val bucketContribution = ScalarMult.multiply(BigInt(digit), buckets(digit))

          windowSum = windowSum + bucketContribution
          digit = digit + 1
        }

        // shifts the whole window back into the right scalar position
        // e.g window = 3 windowIndex = 2
        // multiplier = 2^(3*2) = 64
        // so third window contributes value to 64 place
        val shiftedWindowSum = ScalarMult.multiply(BigInt(1) << (window * windowIndex), windowSum)

        result = result + shiftedWindowSum
        windowIndex = windowIndex + 1
      }

      result
    }
  }

  // Scalar decomposition
  // example scalar = 83 and window = 3
  // split scalar 83 into 3-bits chunks
  // 2^3 = 8 so read scalar in base 8
  // 83 = 3 + 2*8 + 1*64 = 3*8^0 + 2*8^1 + 1*8^2
  // scalarWindowCount computes
  // How many fixed-size windows do I need to cover this scalar
  private def scalarWindowCount(scalar: BigInt, window: Int): Int = {
    require(scalar >= 0, s"scalar must be non-negative, got $scalar")

    if (scalar == 0) 0
    else ((scalar.bitLength + window - 1) / window)
  }

  // extracting one chunk from one scalar
  // 83 = 1010011 base 2
  // 3 bit windows
  // 001 | 010 | 011
  //  1     2     3
  // windowDigit(83, 3, 0) = 3
  // windowDigit(83, 3, 1) = 2
  // windowDigit(83, 3, 2) = 1
  private def windowDigit(scalar: BigInt, window: Int, windowIndex: Int): BigInt = {
    require(scalar >= 0, s"scalar must be non-negative, got $scalar")
    require(window > 0, s"window must be positive, got $window")
    require(windowIndex >= 0, s"windowIndex must be non-negative, got $windowIndex")

    val mask = (BigInt(1) << window) - 1 // moves the desired chunk down to the lowest bits
    (scalar >> (window * windowIndex)) & mask
  }
}
