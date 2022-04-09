package study.stack

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QueueWithStacksTest extends AnyFlatSpec with Matchers {

  "MyQueue" should "correctly implement queue FIFO" in {
    val queue = new QueueWithStacks.MyQueue()
    queue.push(1)
    queue.push(2)
    queue.peek() shouldBe 1
    queue.pop() shouldBe 1
    queue.peek() shouldBe 2
    queue.empty() shouldBe false
    queue.pop() shouldBe 2
    queue.empty() shouldBe true
  }
}
