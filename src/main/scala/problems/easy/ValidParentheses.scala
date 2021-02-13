package problems.easy

object ValidParentheses {
  def main(args: Array[String]): Unit = {
    val s: String = Console.in.readLine.toCharArray.filter(List('(',')','[',']','{','}').contains).mkString
    Console.out.println(isValid(s))
  }

  def isValid(s: String): Boolean = {
    val mm = Map(')' -> '(', ']' -> '[', '}' -> '{')
    val stack = Stack[Char]

    s.toCharArray.foreach { c =>
      if (mm.map{case (k,v) => (v,k)}.contains(c)) stack.put(c)
      else if (mm.contains(c)){
        if (stack.nonEmpty && stack.get == mm(c)) stack.pop else stack.put(c)
      }
    }
    stack.isEmpty
  }

  class Stack[A](private[this] var lst: scala.collection.mutable.ArraySeq[A]) {
    def pop: A = {
      val a = lst.head
      lst = lst.tail
      a
    }
    def put(a: A): Unit = {
      lst = a +: lst
    }
    def get: A = lst.head
    def nonEmpty: Boolean = lst.nonEmpty
    def isEmpty: Boolean = lst.isEmpty
    def show: scala.collection.mutable.ArraySeq[A] = lst
  }

  object Stack {
    def apply[A: scala.reflect.ClassTag]: Stack[A] = new Stack(scala.collection.mutable.ArraySeq.empty[A])
  }
}
