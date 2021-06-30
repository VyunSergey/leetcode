package problems.medium

object ReverseLinkedListTwo {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def main(args: Array[String]): Unit = {
    val lls = Console.in.readLine().split(" ").map(_.toInt).toList
    val Array(left, right) = Console.in.readLine().split(" ").map(_.toInt).take(2)

    assert(left <= right)

    val res = reverseBetween(lls.toListNode(), left, right)
    println(res.toListL())
  }

  def reverseBetween(head: ListNode, left: Int, right: Int): ListNode = {
    val list = head.toListR()
    println(list)
    val (from, between, after) = (list.take(left - 1), list.slice(left - 1, right), list.takeRight(list.length - right))
    println(from)
    println(between)
    println(after)
    (after.reverse ++ between ++ from.reverse).toListNode()
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
