package crypto.kata

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@State(Scope.Benchmark)
class MsmBenchmark {
  @Param(Array("8", "32", "128", "512"))
  var size: Int = 0

  @Param(Array("2", "3", "4", "5"))
  var window: Int = 0

  private var terms: Vector[Msm.Term] =
    Vector.empty

  @Setup(Level.Trial)
  def setup(): Unit = {
    val rng =
      new Random(0L)

    val nonInfinityPoints =
      Curve.points.collect {
        case p @ Point.Affine(_, _) => p
      }

    require(nonInfinityPoints.nonEmpty, "benchmark needs affine points")

    terms =
      Vector.fill(size) {
        val scalar =
          BigInt(128, rng).abs

        val point =
          nonInfinityPoints(rng.nextInt(nonInfinityPoints.length))

        Msm.Term(scalar, point)
      }
  }

  @Benchmark
  def naiveMsm(blackhole: Blackhole): Unit = {
    blackhole.consume(Msm.naive(terms))
  }

  @Benchmark
  def bucketedDirectMsm(blackhole: Blackhole): Unit = {
    blackhole.consume(Msm.bucketedDirect(terms, window))
  }

  @Benchmark
  def bucketedPippengerMsm(blackhole: Blackhole): Unit = {
    blackhole.consume(Msm.bucketedPippenger(terms, window))
  }
}
