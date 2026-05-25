package crypto.kata

import zio.test.*

object PedersenSpec extends ZIOSpecDefault {
  private val g: Point = GroupUtil.findPointWithOrderAtLeast(20)

  private val order: BigInt = GroupUtil.orderOf(g)

  private val alpha: BigInt =
    GroupUtil.findInvertibleScalarMod(order)

  private val h: Point =
    ScalarMult.multiply(alpha, g)

  private val params: Pedersen.Parameters =
    Pedersen.Parameters(g = g, h = h, order = order)

  private val genScalar: Gen[Any, BigInt] =
    Gen.bigInt(BigInt(0), order - 1)

  private val genNonZeroScalar: Gen[Any, BigInt] = Gen.bigInt(BigInt(1), order - 1)

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
            Msm.bucketedPippenger(
              terms = List(
                Msm.Term(opening.message.mod(order), params.g),
                Msm.Term(opening.randomness.mod(order), params.h)
              ),
              window = 3
            )

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

      test("different randomness changes commitment for fixed message") {
        check(genScalar, genScalar, genNonZeroScalar) { (message, randomness, delta) =>
          val first =
            Pedersen.Opening(
              message = message,
              randomness = randomness
            )

          val second =
            Pedersen.Opening(
              message = message,
              randomness = (randomness + delta).mod(order)
            )

          assertTrue(
            Pedersen.commit(params, first) != Pedersen.commit(params, second)
          )
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

      test("forged opening can use a different message") {
        check(genOpening, genNonZeroScalar) { (original, delta) =>
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
