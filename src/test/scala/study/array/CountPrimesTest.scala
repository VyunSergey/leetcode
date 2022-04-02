package study.array

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountPrimesTest extends AnyFlatSpec with Matchers {

  "countPrimes" should "correctly count prime numbers for 5" in {
    val n = 5
    CountPrimes.countPrimes(n) shouldBe 2
  }

  it should "correctly count prime numbers for 17" in {
    val n = 17
    CountPrimes.countPrimes(n) shouldBe 6
  }

  it should "correctly count prime numbers for 100500" in {
    val n = 100500
    CountPrimes.countPrimes(n) shouldBe 9632
  }
}
