package study.stack

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidParenthesesTest extends AnyFlatSpec with Matchers {

  "isValid" should "correctly validate parentheses (" in {
    ValidParentheses.isValid("(") shouldBe false
  }

  it should "correctly validate parentheses )" in {
    ValidParentheses.isValid(")") shouldBe false
  }

  it should "correctly validate parentheses ()" in {
    ValidParentheses.isValid("()") shouldBe true
  }

  it should "correctly validate parentheses (())" in {
    ValidParentheses.isValid("(())") shouldBe true
  }

  it should "correctly validate parentheses ()()" in {
    ValidParentheses.isValid("()()") shouldBe true
  }

  it should "correctly validate parentheses )()" in {
    ValidParentheses.isValid(")()") shouldBe false
  }

  it should "correctly validate parentheses ())" in {
    ValidParentheses.isValid("())") shouldBe false
  }

  it should "correctly validate parentheses (()(()))(()(())((())()()))" in {
    ValidParentheses.isValid("(()(()))(()(())((())()()))") shouldBe true
  }

  it should "correctly validate parentheses ((())(()()())(()())" in {
    ValidParentheses.isValid("((())(()()())(()())") shouldBe false
  }
}
