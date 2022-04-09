package study.stack

import scala.collection.mutable

object ValidParentheses {
  private val open = Array('(', '[', '{')
  private val close = Array(')', ']', '}')

  def main(args: Array[String]): Unit = {
    val str: String = Console.in.readLine().collect { case x if (open ++ close).contains(x) => x }
    println(str)
    val res = isValid(str)
    println(res)
  }

  def isValid(s: String): Boolean = {
    val stack = mutable.Stack.empty[Char]
    val map = Map(')' -> '(', ']' -> '[', '}' -> '{')
    s.foreach { c =>
      if (open.contains(c)) stack.push(c)
      else if (close.contains(c)) {
        if (stack.isEmpty || map(c) != stack.pop()) return false
      }
    }
    stack.isEmpty
  }
}
