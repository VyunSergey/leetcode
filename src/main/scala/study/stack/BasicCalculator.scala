package study.stack

import scala.collection.mutable

object BasicCalculator {
  private val chars = ('0' to '9').toArray ++ Array('+', '-', '(', ')', ' ')

  def main(args: Array[String]): Unit = {
    println(chars.toList)
    val str = Console.in.readLine().collect { case x if chars.contains(x) => x }
    println(str)

    val res = calculate(str)
    println(res)
  }

  // TODO implement method
  def calculate(s: String): Int = {
    val stack = mutable.Stack.empty[Int]
    stack.size
  }
}
