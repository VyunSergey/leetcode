package problems.hard

import scala.collection.mutable

object LongestValidParentheses {
  def main(args: Array[String]): Unit = {
    val s: String = Console.in.readLine.toCharArray
      .filter(List('(',')').contains).mkString
    //val s = ")(((((()())()()))()(()))("
    Console.out.println(longestValidParentheses(s))
  }

  def longestValidParentheses(s: String): Int = {
    val stack = mutable.Stack.empty[Int]
    stack.push(-1)

    var length = 0
    for {
      i <- 0 until s.length
    } yield {
      if (s.charAt(i) == '(') stack.push(i)
      else {
        if (stack.nonEmpty) stack.pop()
        if (stack.nonEmpty) length = Math.max(length, i - stack.top)
        else {
          stack.push(i)
        }
      }
    }
    length
  }
}
