package crypto.kata

object GroupUtil {
  def orderOf(p: Point): BigInt = {
    require(p != Point.Infinity, "infinity does not have a useful generator order")

    var acc = p
    var n = BigInt(1)
    while (acc != Point.Infinity) {
      acc = acc + p
      n = n + 1
    }

    n
  }

  def findPointWithOrderAtLeast(minOrder: BigInt): Point =
    Curve.points.collectFirst {
      case p @ Point.Affine(_, _) if orderOf(p) >= minOrder => p
    }.getOrElse {
      throw new IllegalStateException(s"no point with order >= $minOrder found")
    }

  def scalarMod(k: BigInt, order: BigInt): BigInt = k.mod(order)

  def inverseModUnsafe(k: BigInt, modulus: BigInt): BigInt =
    Field.modInverse(k.mod(modulus), modulus)

  def findInvertibleScalarMod(modulus: BigInt): BigInt =
    (BigInt(2) until modulus).find(_.gcd(modulus) == 1).getOrElse {
      throw new IllegalStateException(s"could not find invertible scalar modulo $modulus")
    }
}