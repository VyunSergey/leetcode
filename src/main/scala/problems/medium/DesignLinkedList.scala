package problems.medium

import scala.collection.mutable

object DesignLinkedList {
  class MyLinkedList() {
    val buffer: mutable.ListBuffer[Int] = mutable.ListBuffer.empty[Int]

    // buffer
    // [2,1,3,4]
    // 2->1->3->4
    def get(index: Int): Int = {
      if (0 <= index && index < buffer.length) buffer(index)
      else -1
    }

    // 2 +=: [1] => [2,1]
    def addAtHead(`val`: Int): Unit = {
      `val` +=: buffer
    }

    // [3] += 4 => [3,4]
    def addAtTail(`val`: Int): Unit = {
      buffer += `val`
    }

    def addAtIndex(index: Int, `val`: Int): Unit = {
      if (0 <= index && index < buffer.length) buffer.insert(index, `val`)
      else if (index == buffer.length) buffer += `val`
    }

    def deleteAtIndex(index: Int): Unit = {
      if (0 <= index && index < buffer.length) buffer.remove(index)
    }
  }

  def main(args: Array[String]): Unit = {
    val myLinkedList = new DesignLinkedList.MyLinkedList

    def printAndCheck[T](value: => T, expected: T): Unit = {
      println(value)
      assert(value == expected)
    }

    // linked list becomes 1
    myLinkedList.addAtHead(1)
    // return 1
    printAndCheck(myLinkedList.get(0), 1)
    // linked list becomes 1->3
    myLinkedList.addAtTail(3)
    // return 1
    printAndCheck(myLinkedList.get(0), 1)
    // return 3
    printAndCheck(myLinkedList.get(1), 3)
    // linked list becomes 1->2->3
    myLinkedList.addAtIndex(1, 2)
    // return 1
    printAndCheck(myLinkedList.get(0), 1)
    // return 2
    printAndCheck(myLinkedList.get(1), 2)
    // return 3
    printAndCheck(myLinkedList.get(2), 3)
    // now the linked list is 1->3
    myLinkedList.deleteAtIndex(1)
    // return 1
    printAndCheck(myLinkedList.get(0), 1)
    // return 3
    printAndCheck(myLinkedList.get(1), 3)
  }
}
