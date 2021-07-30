package problems.hard

import scala.collection.mutable

object BasicCalculator {
  def main(args: Array[String]): Unit = {
    val expression: String = Console.in.readLine()
      .filter(c => c.isDigit || c.isSpaceChar || List('+', '-', '(', ')').contains(c))
    val res: Int = calculate(expression)
    println(res)
  }

  def calculate(s: String): Int = {
    val stack = mutable.Stack.empty[Int]
    var result = 0
    var number = 0
    var sign = 1
    var c = '0'

    (0 until s.length).foreach { i =>
      c = s(i)
      if (c.isDigit) {
        number = 10 * number + (c - '0')
      } else if (c == '+') {
        result += sign * number
        number = 0
        sign = 1
      } else if (c == '-') {
        result += sign * number
        number = 0
        sign = -1
      } else if (c == '(') {
        // push the result first, then sign;
        stack.push(result)
        stack.push(sign)
        // reset the sign and result for the value in the parenthesis
        sign = 1
        result = 0
      } else if(c == ')') {
        result += sign * number
        number = 0
        // stack.pop() is the sign before the parenthesis
        result *= stack.pop()
        // stack.pop() now is the result calculated before the parenthesis
        result += stack.pop()
      }
    }
    if (number != 0) result += sign * number
    result
  }
}
