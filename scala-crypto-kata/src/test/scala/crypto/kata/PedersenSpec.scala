package crypto.kata

import zio.test.*

object PedersenSpec extends ZIOSpecDefault {
  private val g: Point = GroupUtil.findPointWithOrderAtLeast(20)

  private val order: BigInt = GroupUtil.orderOf(g)

  private val alpha: BigInt = {
    // Choose α invertible modulo order.
    //
    // For a toy curve, order may be composite, so avoid assuming every
    // non-zero scalar is invertible.
    val candidate = (BigInt(2) until order).find(_.gcd(order) == 1)

    candidate.getOrElse {
      throw new IllegalStateException(s"could not find invertible alpha modulo $order")
    }
  }

  private val h: Point = ScalarMult.multiply(alpha, g)

  private val params: Pedersen.Parameters = Pedersen.Parameters(g = g, h = h, order = order)

  private val genScalar: Gen[Any, BigInt] = Gen.bigInt(BigInt(0), order - 1)

  private val genOpening: Gen[Any, Pedersen.Opening] =
    for {
      m <- genScalar
      r <- genScalar
    } yield Pedersen.Opening(m, r)

  override def spec: Spec[TestEnvironment, Any] =
    suite("Pedersen commitment using MSM")(
      test("same opening gives same commitment") {
        check(genOpening) { opening =>
          assertTrue(
            Pedersen.commit(params, opening) ==
              Pedersen.commit(params, opening)
          )
        }
      },

      test("commitment is computed as mG + rH") {
        check(genOpening) { opening =>
          val expected =
            Msm.naive(List(
              Msm.Term(opening.message, params.g),
              Msm.Term(opening.randomness, params.h)
            ))

          assertTrue(
            Pedersen.commit(params, opening) == expected
          )
        }
      },

      test("commitment is homomorphic over openings") {
        check(genOpening, genOpening) { (x, y) =>
          val lhs = Pedersen.commit(params, Pedersen.addOpenings(params, x, y))
          val rhs = Pedersen.commit(params, x) + Pedersen.commit(params, y)

          assertTrue(lhs == rhs)
        }
      },

      test("binding fails when H = αG and α is known") {
        check(genOpening, genScalar) { (original, newMessage) =>
          val forged =
            Pedersen.forgeOpeningWhenRelationKnown(
              params = params,
              alpha = alpha,
              original = original,
              newMessage = newMessage
            )

          assertTrue(
            forged.message == newMessage.mod(order),
            Pedersen.commit(params, original) ==
              Pedersen.commit(params, forged)
          )
        }
      },

      test("forged opening may use a different message") {
        check(genOpening, genScalar.filter(m => m != BigInt(0))) { (original, delta) =>
          val newMessage = (original.message + delta).mod(order)

          val forged = Pedersen.forgeOpeningWhenRelationKnown(
              params = params,
              alpha = alpha,
              original = original,
              newMessage = newMessage
            )

          assertTrue(
            newMessage != original.message.mod(order),
            Pedersen.commit(params, original) ==
              Pedersen.commit(params, forged)
          )
        }
      }
    )
}
