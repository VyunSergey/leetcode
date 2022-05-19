package problems.easy

import problems.common.ListNode

object MergeTwoSortedLists {
  def main(args: Array[String]): Unit = {
    val l1 = toListNode(Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}.toList.reverse)
    val l2 = toListNode(Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}.toList.reverse)
    Console.out.println(toListR(mergeTwoLists(l1, l2)).mkString("[", ",", "]"))
  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    toListNode(toListR(merge(l1, l2, null)))
  }

  @scala.annotation.tailrec
  def merge(l1: ListNode, l2: ListNode, acc: ListNode): ListNode = {
    if (l1 == null && l2 == null) acc
    else if (l1 == null) merge(l1, l2.next, new ListNode(l2.x, acc))
    else if (l2 == null) merge(l1.next, l2, new ListNode(l1.x, acc))
    else if (l1.x > l2.x) merge(l1, l2.next, new ListNode(l2.x, acc))
    else merge(l1.next, l2, new ListNode(l1.x, acc))
  }

  @scala.annotation.tailrec
  def toListL(ln: ListNode, acc: List[Int] = Nil): List[Int] = {
    if (ln == null) acc
    else toListL(ln.next, ln.x +: acc)
  }

  @scala.annotation.tailrec
  def toListR(ln: ListNode, acc: List[Int] = Nil): List[Int] = {
    if (ln == null) acc
    else toListR(ln.next, acc :+ ln.x)
  }

  @scala.annotation.tailrec
  def toListNode(l: List[Int], acc: ListNode = null): ListNode = {
    if (l.isEmpty) acc
    else toListNode(l.tail, new ListNode(l.head, acc))
  }
}
