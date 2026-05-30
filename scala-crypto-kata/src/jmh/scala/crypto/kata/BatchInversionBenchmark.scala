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
class BatchInversionBenchmark {
  @Param(Array("1", "2", "4", "8", "16", "32", "64", "128"))
  var size: Int = 1

  private var values: Vector[Field] =
    Vector.empty

  @Setup(Level.Trial)
  def setup(): Unit = {
    val rng =
      new Random(0L)

    values =
      Vector.fill(size) {
        var x = Field.zero

        while (x == Field.zero) {
          x = Field(BigInt(32, rng))
        }

        x
      }
  }

  @Benchmark
  def individualInverses(blackhole: Blackhole): Unit = {
    blackhole.consume(values.map(_.inverseNonZero))
  }

  @Benchmark
  def batchInverses(blackhole: Blackhole): Unit = {
    blackhole.consume(Field.batchInverseNonZero(values))
  }
}
