package problems.medium

import problems.common.TreeNode
import scala.collection.mutable

object LowestCommonAncestorBinaryTree {
  def main(args: Array[String]): Unit = {
    val tree: TreeNode =
      TreeNode(0,
        TreeNode(1,
          TreeNode(3,
            TreeNode(7, null, null), null),
          TreeNode(4,
            null, TreeNode(8, TreeNode(11, null, null), null))
        ),
        TreeNode(2,
          TreeNode(5,
            TreeNode(9, null, null), null),
          TreeNode(6,
            null, TreeNode(10, null, TreeNode(12, null, null)))
        )
      )
    val p = Console.in.readLine().toInt
    val q = Console.in.readLine().toInt
    val res: TreeNode = lowestCommonAncestor(tree, TreeNode(p), TreeNode(q))
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
