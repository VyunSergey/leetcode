package problems.easy

object ValidParentheses {
  def main(args: Array[String]): Unit = {
    val s: String = Console.in.readLine.toCharArray
      .filter(List('(',')','[',']','{','}').contains).mkString
    Console.out.println(isValid(s))
  }

  def isValid(s: String): Boolean = {
    val mm = Map(')' -> '(', ']' -> '[', '}' -> '{')
    val stack = scala.collection.mutable.Stack.empty[Char]

    s.toCharArray.foreach { c =>
      if (mm.map{case (k,v) => (v,k)}.contains(c)) stack.push(c)
      else if (stack.headOption.contains(mm(c))) {
        stack.pop()
      }
      else {
        return false
      }
    }
    stack.isEmpty
  }
}
