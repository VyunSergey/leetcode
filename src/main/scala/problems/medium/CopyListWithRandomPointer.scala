package problems.medium

import scala.collection.mutable

object CopyListWithRandomPointer {
  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }

  object Node {
    def apply(value: Int): Node = new Node(value)
    def apply(value: Int, next: Node): Node = {
      val node = Node(value)
      node.next = next
      node
    }
    def apply(value: Int, next: Node, random: Node): Node = {
      val node = Node(value)
      node.next = next
      node.random = random
      node
    }
    def show(node: Node): String = {
      if (node != null) s"Node(${node.value},${show(node.next)})"
      else "null"
    }
  }

  def main(args: Array[String]): Unit = {
    val head: Node = Node(7, Node(13, Node(11, Node(10, Node(1)))))
    head.next.next.next.next.random = head
    head.next.next.next.random = head.next.next
    head.next.next.random = head.next.next.next.next
    head.next.random = head
    val res: Node = copyRandomList(head)
    println(Node.show(res))
  }

  val mapOldToNew = mutable.Map.empty[Node, Node]

  def searchNodes(node: Node): Unit = {
    if (node != null && !mapOldToNew.contains(node)) {
      mapOldToNew += (node -> new Node(node.value))
      searchNodes(node.next)
      searchNodes(node.random)
    }
  }

  def setupPointers(map: mutable.Map[Node, Node]): Unit = {
    map.foreachEntry { (oldNode, newNode) =>
      map.get(oldNode.next).foreach { node =>
        newNode.next = node
      }
      map.get(oldNode.random).foreach { node =>
        newNode.random = node
      }
    }
  }

  def copyRandomList(head: Node): Node = {
    searchNodes(head)
    setupPointers(mapOldToNew)
    mapOldToNew.get(head).orNull
  }
}
