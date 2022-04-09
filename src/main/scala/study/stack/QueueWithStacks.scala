package study.stack

import scala.collection.mutable

object QueueWithStacks {
  def main(args: Array[String]): Unit = {
    val queue = new MyQueue()
    queue.push(1)
    queue.push(2)
    println(queue.peek())
    println(queue.pop())
    println(queue.peek())
    println(queue.empty())
    println(queue.pop())
    println(queue.empty())
  }

  class MyQueue() {
    val input: mutable.Stack[Int] = mutable.Stack.empty[Int]
    val output: mutable.Stack[Int] = mutable.Stack.empty[Int]

    def push(x: Int): Unit = {
      input.push(x)
    }

    def pop(): Int = {
      peek()
      output.pop()
    }

    def peek(): Int = {
      if (output.isEmpty) {
        while(input.nonEmpty) output.push(input.pop())
      }
      output.top
    }

    def empty(): Boolean = {
      input.isEmpty && output.isEmpty
    }
  }
}
