package study.stack

import scala.collection.mutable

object LongestValidParentheses {
  private val open = '('
  private val close = ')'

  def main(args: Array[String]): Unit = {
    val str = Console.in.readLine().collect { case x if Array(open, close).contains(x) => x }
    println(str)
    val res = longestValidParentheses(str)
    println(res)
  }

  def longestValidParentheses(s: String): Int = {
    val stack = mutable.Stack.empty[Int]
    var max = 0

    stack.push(-1)
    // println((-1, max, stack.toList))
    s.indices.foreach { i =>
      if (s(i) == '(') stack.push(i)
      else {
        if (stack.nonEmpty) stack.pop()
        if (stack.nonEmpty) max = Math.max(max, i - stack.top)
        if (stack.isEmpty) stack.push(i)
      }
      // println((i, max, stack.toList))
    }
    max
  }
}
