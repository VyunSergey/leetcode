package problems.medium

object TreeLevelOrderTraversal {
  class Node(var _value: Int) {
    var value: Int = _value
    var children: List[Node] = List()
  }

  object Node {
    def apply(value: Int): Node = new Node(value)
    def apply(value: Int, children: List[Node]): Node = {
      val node = new Node(value)
      node.children = children
      node
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Node = Node(1,
      List(
        Node(3, List(Node(5),
                     Node(6))),
        Node(2, List(Node(9, List(Node(10, List(Node(11))))))),
        Node(4, List(Node(7, List(Node(8))))))
    )
    val res: List[List[Int]] = levelOrder(tree)
    println(res.map(_.mkString("[", ", ", "]")).mkString("\n"))
  }

  def levelOrder(root: Node): List[List[Int]] = {
    if (root == null) return List.empty[List[Int]]
    List(List(root.value)) ++
      root.children.foldLeft(List.empty[List[Int]]) { (acc, node) =>
        zipLists(acc, levelOrder(node))
      }
  }

  def zipLists(ls1: List[List[Int]], ls2: List[List[Int]]): List[List[Int]] = {
    List.tabulate(Math.max(ls1.length, ls2.length)) { i =>
      (if (i < ls1.length) ls1(i) else List.empty[Int]) ++
        (if (i < ls2.length) ls2(i) else List.empty[Int])
    }
  }
}
