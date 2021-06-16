package problems.hard

import common._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class MinimumWindowSubstringTest extends AnyFlatSpec with Matchers {
  def genSymbol(chars: List[Char]): String = {
    val ind = Random.nextInt(chars.length)
    chars(ind).toString
  }

  def genString(n: Int): String = {
    List.fill(n)(genSymbol(List.range('a', 'z')))
      .foldLeft("")(_ + _)
  }

  def genStringWithSubstring(subStr: String)(n: Int): String = {
    genString(n / 2).replaceAll(subStr.head.toString, "") +
      subStr +
      genString(n / 2).replaceAll(subStr.head.toString, "")
  }

  def test(s: String, t: String, expected: String): Assertion = {
    MinimumWindowSubstring.minWindow(s, t) shouldBe expected
  }

  def pow10(n: Int): Double = Math.pow(10, n + 1)

  val data: List[(String, String)] =
    for {
      sub <- List.tabulate(5)(i => genString(pow10(i).toInt))
      str <- List.tabulate(5)(i => genStringWithSubstring(sub)(pow10(i).toInt))
    } yield {
      (str, sub)
    }

  "minWindow" should "correctly find substring" in {
    val str = "gehzduwqkzuyotckqcusdiqubeqglkvuocttzrllqfjhzorpqnjwxbqyfiesscmigicfzn"
    val sub = "qsvczwsslkhwg"
    println(str)
    println(sub)
    val res = MinimumWindowSubstring.minWindow(str, sub)
    println(res)
    res shouldBe "wqkzuyotckqcusdiqubeqglkvuocttzrllqfjhzorpqnjwxbqyfiess"
  }

  "minWindow" should "correctly find all substrings" in {
    data.foreach { case (s, t) =>
      println(f"${s.length}%10s, ${cut(s)(10)}%10s, ${t.length}%10s, ${cut(t)(10)}%10s")
    }
    val fun1 = (tp: (String, String)) => MinimumWindowSubstring.minWindow(tp._1, tp._2)
    val prof = profileStats(fun1, fun1)(data)
    println("--------")
    println(prof._2)
    println(prof._3)
    println(prof._4)
    println("--------")
    println(prof._5)
    println(prof._6)
    println(prof._7)
    prof._1 shouldBe true
  }
}
