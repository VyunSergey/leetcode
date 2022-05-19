package problems.medium

import problems.common.ListNode
import scala.annotation.tailrec

object AddTwoNumbers {
  def main(args: Array[String]): Unit = {
    val l1 = fromList(Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}.toList.reverse)
    val l2 = fromList(Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}.toList.reverse)
    Console.out.println(toList(addTwoNumbers(l1, l2)).mkString("[", ",", "]"))
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    @tailrec
    def addRec(l1: ListNode, l2: ListNode, digit: Int, acc: ListNode = null): ListNode = {
      if (l1 == null && l2 == null) {
        if (digit > 0) new ListNode(digit, acc)
        else acc
      }
      else if (l1 == null) {
        val sum = (l2.x + digit) % 10
        val d = (l2.x + digit) / 10
        val res = new ListNode(sum, acc)
        addRec(l1, l2.next, d, res)
      }
      else if (l2 == null) {
        val sum = (l1.x + digit) % 10
        val d = (l1.x + digit) / 10
        val res = new ListNode(sum, acc)
        addRec(l1.next, l2, d, res)
      }
      else {
        val sum = (l1.x + l2.x + digit) % 10
        val d = (l1.x + l2.x + digit) / 10
        val res = new ListNode(sum, acc)
        addRec(l1.next, l2.next, d, res)
      }
    }

    fromList(toList(addRec(l1, l2, 0)))
  }

  @tailrec
  def toList(l: ListNode, acc: List[Int] = List.empty[Int]): List[Int] = {
    if (l == null) acc
    else toList(l.next, acc :+ l.x)
  }

  @tailrec
  def fromList(l: List[Int], acc: ListNode = null): ListNode = {
    if (l.isEmpty) acc
    else fromList(l.tail, new ListNode(l.head, acc))
  }
}
