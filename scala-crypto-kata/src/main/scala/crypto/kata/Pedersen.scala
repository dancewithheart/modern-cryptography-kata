package crypto.kata

object Pedersen {
  final case class Parameters(g: Point, h: Point, order: BigInt) {
    require(g != Point.Infinity, "g must not be infinity")
    require(h != Point.Infinity, "h must not be infinity")
    require(order > 1, s"order must be greater than 1, got $order")
  }

  final case class Opening(message: BigInt, randomness: BigInt)

  def commit(params: Parameters, opening: Opening): Point = {
    val m = opening.message.mod(params.order)
    val r = opening.randomness.mod(params.order)

    Msm.bucketedPippenger(
      terms = List(
        Msm.Term(m, params.g),
        Msm.Term(r, params.h)
      ),
      window = 3
    )
  }

  def addOpenings(params: Parameters, x: Opening, y: Opening): Opening =
    Opening(
      message = (x.message + y.message).mod(params.order),
      randomness = (x.randomness + y.randomness).mod(params.order)
    )

  def forgeOpeningWhenRelationKnown(params: Parameters, alpha: BigInt, original: Opening, newMessage: BigInt): Opening = {
    // This models the bad setup:
    //   H = αG
    //
    // Then:
    //   Com(m, r) = mG + rH
    //             = mG + rαG
    //             = (m + αr)G
    //
    // To open the same commitment as m', choose r' such that:
    //   m + αr = m' + αr' mod order
    //
    // Therefore:
    //   r' = (m + αr - m') / α mod order
    val alphaMod = alpha.mod(params.order)

    val alphaInv = GroupUtil.inverseModUnsafe(alphaMod, params.order)

    val originalLinearCombination =
      (original.message + alphaMod * original.randomness).mod(params.order)

    val newRandomness =
      ((originalLinearCombination - newMessage) * alphaInv).mod(params.order)

    Opening(
      message = newMessage.mod(params.order),
      randomness = newRandomness
    )
  }
}
