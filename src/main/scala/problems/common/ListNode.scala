package problems.common

import scala.annotation.tailrec

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object ListNode {
  def apply(x: Int): ListNode = new ListNode(x)
  def apply(x: Int, next: ListNode): ListNode = new ListNode(x, next)

  @tailrec
  def show(head: ListNode, str: String = ""): String = {
    if (head == null) str
    else show(head.next, if (str != "") str + s"->${head.x}" else s"${head.x}")
  }

  implicit class ListNodeOps(ln: ListNode) {
    @tailrec
    final def toListL(acc: List[Int] = List.empty[Int]): List[Int] = {
      if (ln == null) acc
      else ln.next.toListL(ln.x +: acc)
    }

    @tailrec
    final def toListR(acc: List[Int] = List.empty[Int]): List[Int] = {
      if (ln == null) acc
      else ln.next.toListR(acc :+ ln.x)
    }
  }
}
