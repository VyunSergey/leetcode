package problems.medium

import problems.common.ListNode
import scala.annotation.tailrec

object RemoveNthNodeFromEndOfList {
  def main(args: Array[String]): Unit = {
    val head = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}.toList.reverse.toListNode()
    val n = Console.in.readLine.toInt
    val res = removeNthFromEnd(head, n).toListR().mkString("[", ",", "]")
    println(res)
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    head.toListL().zipWithIndex.filterNot(_._2 == n - 1).map(_._1).toListNode()
  }

  implicit class ListOps(lst: List[Int]) {
    @tailrec
    final def toListNode(acc: ListNode = null): ListNode = {
      if (lst.isEmpty) acc
      else lst.tail.toListNode(new ListNode(lst.head, acc))
    }
  }
}
