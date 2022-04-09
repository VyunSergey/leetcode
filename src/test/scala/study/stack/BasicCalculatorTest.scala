package study.stack

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicCalculatorTest extends AnyFlatSpec with Matchers {

  "calculate" should "correctly calculate expression -12345" in {
    BasicCalculator.calculate("-12345") shouldBe -12345
  }

  it should "correctly calculate expression 1+2" in {
    BasicCalculator.calculate("1+2") shouldBe 3
  }

  it should "correctly calculate expression 3-7" in {
    BasicCalculator.calculate("3-7") shouldBe -4
  }

  it should "correctly calculate expression (-(1 + 3)-7)+10" in {
    BasicCalculator.calculate("(-(1 + 3)-7)+10") shouldBe -1
  }

  it should "correctly calculate expression (-1+(2 - 3)-9)-(0-89234+13)" in {
    BasicCalculator.calculate("(-1+(2 - 3)-9)-(0-89234+13)") shouldBe 89210
  }

  it should "correctly calculate expression -132424-(1+(3443424-13313)-242442)" in {
    BasicCalculator.calculate("-132424-(1+(3443424-13313)-242442)") shouldBe -3320094
  }
}
