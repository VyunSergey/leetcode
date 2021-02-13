package problems.medium

import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import problems.easy.ValidParentheses

import scala.annotation.tailrec

class GenerateParenthesesTest extends AnyFlatSpec with Matchers {
  def genDummy(n: Int): List[String] = {
    def combine(s1: Set[String], s2: Set[String]): Set[String] = {
      for {
        a <- s1
        b <- s2
      } yield a + b
    }

    @tailrec
    def genAll(n: Int, acc: Set[String] = Set("")): Set[String] = {
      if (n == 0) acc
      else genAll(n - 1, combine(acc, Set("))", ")(", "()", "((")))
    }

    genAll(n).filter(ValidParentheses.isValid).toList
  }

  def test(n: Int, expected: List[String]): Assertion = {
    GenerateParentheses.generateParenthesis(n).sorted shouldBe expected.sorted
  }

  "GenerateParentheses" should "correctly generate parentheses for n=0" in {
    test(0, List(""))
  }

  it should "correctly generate parentheses for n=1" in {
    test(1, List("()"))
  }

  it should "correctly generate parentheses for n=2" in {
    test(2, List("()()", "(())"))
  }

  it should "correctly generate parentheses for n=3" in {
    test(3, List("()()()", "()(())", "(())()", "(()())", "((()))"))
  }

  it should "correctly generate parentheses for n=4" in {
    test(4, genDummy(4))
  }

  it should "correctly generate parentheses for n=5" in {
    test(5, genDummy(5))
  }

  it should "correctly generate parentheses for n=6" in {
    test(6, genDummy(6))
  }

  it should "correctly generate parentheses for n=7" in {
    test(7, genDummy(7))
  }

  it should "correctly generate parentheses for n=8" in {
    test(8, genDummy(8))
  }
}
