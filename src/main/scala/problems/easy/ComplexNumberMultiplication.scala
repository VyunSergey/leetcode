package problems.easy

import scala.util.matching.Regex

object ComplexNumberMultiplication {

  case class ComplexNumber(rl: Int, im: Int) {
    def show: String = s"$rl+${im}i"
    def +(other: ComplexNumber): ComplexNumber =
      ComplexNumber(rl + other.rl, im + other.im)
    def *(other: ComplexNumber): ComplexNumber =
      ComplexNumber(rl * other.rl - im * other.im, rl * other.im + im * other.rl)
  }

  object ComplexNumber {
    val pattern: Regex = """([-]*\d+)\+([-]*\d+)i""".r

    def apply(str: String): ComplexNumber = str match {
      case pattern(rl, im) => ComplexNumber(rl.toInt, im.toInt)
      case _ => throw new IllegalArgumentException(s"Cant parse string $str")
    }
  }

  def main(args: Array[String]): Unit = {
    val num1: String = Console.in.readLine()
    val num2: String = Console.in.readLine()
    assert(List(num1, num2).forall(ComplexNumber.pattern.matches))

    val res = complexNumberMultiply(num1, num2)
    println(res)
  }

  def complexNumberMultiply(num1: String, num2: String): String = {
    (ComplexNumber(num1) * ComplexNumber(num2)).show
  }
}
