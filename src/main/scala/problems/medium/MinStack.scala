package problems.medium

object MinStack {
  def main(args: Array[String]): Unit = {
    val stack = new MinStack()
    stack.push(-2)
    println((stack.top(), stack.getMin))
    stack.push(0)
    println((stack.top(), stack.getMin))
    stack.push(-3)
    println((stack.top(), stack.getMin))
    stack.pop()
    println((stack.top(), stack.getMin))
    stack.pop()
    println((stack.top(), stack.getMin))
    stack.pop()
    stack.push(3)
    println((stack.top(), stack.getMin))
  }

  class MinStack() {

    /** initialize your data structure here. */
    var lst: List[Int] = Nil
    var min: Int = Int.MaxValue

    def push(`val`: Int): Unit = {
      lst = `val` :: lst
      min = Math.min(min, `val`)
    }

    def pop(): Unit = {
      val a = lst.head
      lst = lst.tail
      if (min == a) {
        if (lst.nonEmpty) min = lst.min
        else min = Int.MaxValue
      }
    }

    def top(): Int = {
      lst.head
    }

    def getMin: Int = {
      min
    }

  }
}
