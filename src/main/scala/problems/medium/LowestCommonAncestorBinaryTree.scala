package problems.medium

import scala.collection.mutable

object LowestCommonAncestorBinaryTree {
  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  object TreeNode {
    def apply(value: Int, left: TreeNode, right: TreeNode): TreeNode = {
      val tree = new TreeNode(value)
      tree.left = left
      tree.right = right
      tree
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: TreeNode =
      TreeNode(0,
        TreeNode(1,
          TreeNode(3,
            TreeNode(7, null, null), null),
          TreeNode(4,
            null, TreeNode(8, null, null))
        ),
        TreeNode(2,
          TreeNode(5,
            TreeNode(9, null, null), null),
          TreeNode(6,
            null, TreeNode(10, null, null))
        )
      )
    val p = TreeNode(9, null, null)
    val q = TreeNode(10, null, null)
    val res: TreeNode = lowestCommonAncestor(tree, p, q)
    println(res.value)
  }

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    val buffP = mutable.ArrayBuffer.empty[Int]
    val buffQ = mutable.ArrayBuffer.empty[Int]
    val buffT = mutable.ArrayBuffer.empty[TreeNode]

    def findPaths(root: TreeNode, p: Int, q: Int): Unit = {
      if (root != null) {
        val queue = mutable.Queue.empty[(TreeNode, Array[Int])]
        queue.enqueue((root, Array.empty[Int]))
        while(queue.nonEmpty && (buffP.isEmpty || buffQ.isEmpty)) {
          val (node, path) = queue.dequeue()
          if (node.value == p) buffP ++= (path :+ node.value)
          if (node.value == q) buffQ ++= (path :+ node.value)
          if (node.left != null) queue.enqueue((node.left, path :+ node.value))
          if (node.right != null) queue.enqueue((node.right, path :+ node.value))
        }
      }
    }

    def findTree(root: TreeNode, a: Int): Unit = {
      if (root != null) {
        val queue = mutable.Queue.empty[TreeNode]
        queue.enqueue(root)
        while(queue.nonEmpty && buffT.isEmpty) {
          val node = queue.dequeue()
          if (node.value == a) buffT += node
          if (node.left != null) queue.enqueue(node.left)
          if (node.right != null) queue.enqueue(node.right)
        }
      }
    }

    findPaths(root, p.value, q.value)
    println(buffP.toList)
    println(buffQ.toList)

    var i = 1
    val len = Math.min(buffP.length, buffQ.length)
    while(buffP(len - i) != buffQ(len - i)) i += 1
    findTree(root, buffP(len - i))

    buffT.head
  }
}
