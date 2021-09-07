package problems.easy

import scala.annotation.tailrec

object ReverseLinkedList {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object ListNode {
    def apply(x: Int): ListNode = new ListNode(x)
    def apply(x: Int, next: ListNode): ListNode = new ListNode(x, next)

    def show(head: ListNode): String = {
      if (head == null) ""
      else if (head.next == null) s"[${head.x}]"
      else s"[${head.x}, ${show(head.next)}]"
    }
  }

  def main(args: Array[String]): Unit = {
    val ln: ListNode = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
    val res = reverseList(ln)
    println(ListNode.show(res))
  }

  def reverseList(head: ListNode): ListNode = {
    if (head == null || head.next == null) head
    else {
      val tail = reverseList(head.next)
      head.next = null
      if (tail != null) last(tail).next = head
      tail
    }
  }

  @tailrec
  def last(head: ListNode): ListNode = {
    if (head == null || head.next == null) head
    else last(head.next)
  }
}
