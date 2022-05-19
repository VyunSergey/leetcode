package problems.medium

import problems.common.ListNode
import scala.annotation.tailrec

object ReverseLinkedListTwo {
  def main(args: Array[String]): Unit = {
    val lls = Console.in.readLine().split(" ").map(_.toInt).toList
    val Array(left, right) = Console.in.readLine().split(" ").map(_.toInt).take(2)
    assert(left <= right)
    val res = reverseBetween(lls.toListNode(), left, right)
    println(res.toListL())
  }

  def reverseBetween(head: ListNode, left: Int, right: Int): ListNode = {
    val list = head.toListR()
    // println(list)
    val (from, between, after) = (list.take(left - 1), list.slice(left - 1, right), list.takeRight(list.length - right))
    // println(from)
    // println(between)
    // println(after)
    (after.reverse ++ between ++ from.reverse).toListNode()
  }

  implicit class ListOps(lst: List[Int]) {
    @tailrec
    final def toListNode(acc: ListNode = null): ListNode = {
      if (lst.isEmpty) acc
      else lst.tail.toListNode(new ListNode(lst.head, acc))
    }
  }
}
