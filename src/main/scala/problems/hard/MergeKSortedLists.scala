package problems.hard

import problems.common.ListNode
import scala.annotation.tailrec

object MergeKSortedLists {
  @tailrec
  def toList(ln: ListNode, acc: List[Int] = Nil): List[Int] = {
    if (ln == null) acc
    else toList(ln.next, acc :+ ln.x)
  }

  @tailrec
  def toListNode(ll: List[Int], acc: ListNode = null): ListNode = {
    if (ll.isEmpty) acc
    else toListNode(ll.tail, new ListNode(ll.head, acc))
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    def headTail(arr: Array[ListNode]): (ListNode, Array[ListNode]) = {
      val (head, ind) = arr.zipWithIndex.minBy(_._1.x)
      val tail =
        if (head.next == null) arr.zipWithIndex.filterNot(_._2 == ind).map(_._1)
        else arr.updated(ind, head.next)
      (head, tail)
    }

    @tailrec
    def inner(arr: Array[ListNode], acc: ListNode = null): ListNode = {
      if (arr.isEmpty) acc
      else {
        val (head, tail) = headTail(arr)
        inner(tail, new ListNode(head.x, acc))
      }
    }

    toListNode(toList(inner(lists.filterNot(_ == null))))
  }
}
