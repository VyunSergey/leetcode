package problems.medium

object RemoveNthNodeFromEndOfList {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def main(args: Array[String]): Unit = {
    val head = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}.toList.reverse.toListNode()
    val n = Console.in.readLine.toInt
    Console.out.println(removeNthFromEnd(head, n).toListR().mkString("[", ",", "]"))
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    head.toListL().zipWithIndex.filterNot(_._2 == n - 1).map(_._1).toListNode()
  }

  implicit class ListNodeOps(ln: ListNode) {
    @scala.annotation.tailrec
    final def toListL(acc: List[Int] = List.empty[Int]): List[Int] = {
      if (ln == null) acc
      else ln.next.toListL(ln.x +: acc)
    }

    @scala.annotation.tailrec
    final def toListR(acc: List[Int] = List.empty[Int]): List[Int] = {
      if (ln == null) acc
      else ln.next.toListR(acc :+ ln.x)
    }
  }

  implicit class ListOps(lst: List[Int]) {
    @scala.annotation.tailrec
    final def toListNode(acc: ListNode = null): ListNode = {
      if (lst.isEmpty) acc
      else lst.tail.toListNode(new ListNode(lst.head, acc))
    }
  }
}
