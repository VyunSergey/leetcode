package study.stack

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LongestValidParenthesesTest extends AnyFlatSpec with Matchers {

  "longestValidParentheses" should "correctly work for )" in {
    LongestValidParentheses.longestValidParentheses(")") shouldBe 0
  }

  it should "correctly work for (" in {
    LongestValidParentheses.longestValidParentheses("(") shouldBe 0
  }

  it should "correctly work for )(" in {
    LongestValidParentheses.longestValidParentheses(")(") shouldBe 0
  }

  it should "correctly work for ()" in {
    LongestValidParentheses.longestValidParentheses("()") shouldBe 2
  }

  it should "correctly work for )()" in {
    LongestValidParentheses.longestValidParentheses(")()") shouldBe 2
  }

  it should "correctly work for ()(" in {
    LongestValidParentheses.longestValidParentheses("()(") shouldBe 2
  }

  it should "correctly work for ))()(()))))((()((" in {
    LongestValidParentheses.longestValidParentheses("))()(()))))((()((") shouldBe 6
  }

  it should "correctly work for ())))()((())(())())(()))(" in {
    LongestValidParentheses.longestValidParentheses("())))()((())(())())(()))(") shouldBe 18
  }

  it should "correctly work for ()(()(())(())())()))())())())))()))))" in {
    LongestValidParentheses.longestValidParentheses("()(()(())(())())()))())())())))()))))") shouldBe 18
  }

  it should "correctly work for )((()()))()(((((()()(((())(((())()" in {
    LongestValidParentheses.longestValidParentheses(")((()()))()(((((()()(((())(((())()") shouldBe 10
  }
}
