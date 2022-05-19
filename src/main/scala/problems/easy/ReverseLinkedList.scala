package problems.easy

import problems.common.ListNode
import scala.annotation.tailrec

object ReverseLinkedList {
  def main(args: Array[String]): Unit = {
    val ln: ListNode = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
    println(ListNode.show(ln))
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
