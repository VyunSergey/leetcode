package problems.medium

import problems.common.TreeNode

import scala.annotation.tailrec
import scala.collection.mutable

object BinarySearchTreeIterator {
  class BSTIterator(_root: TreeNode) {

    private val lens = mutable.HashMap.empty[Int, Int]

    final private def len(node: TreeNode): Int = {
      if (node != null) {
        lens.getOrElseUpdate(node.value, {
          val left = if (node.left != null) lens.getOrElseUpdate(node.left.value, len(node.left)) else 0
          val right = if (node.right != null) lens.getOrElseUpdate(node.right.value, len(node.right)) else 0
          1 + left + right
        })
      } else 0
    }

    @tailrec
    final private def find(node: TreeNode, ind: Int): Int = {
      // println(s"ind=$ind node=${if (node == null) -1 else node.value}")
      if (node != null) {
        val left = len(node.left)
        if (ind < left) find(node.left, ind)
        else if (ind > left) find(node.right, ind - left - 1)
        else node.value
      } else -1
    }

    private val ln = len(_root)
    private var ind = 0

    def next(): Int = {
      val res = find(_root, ind)
      ind += 1
      res
    }

    def hasNext: Boolean = {
      ind < ln
    }
  }

  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      TreeNode(4,
        TreeNode(2,
          TreeNode(1),
          TreeNode(3),
        ),
        null
      ),
      TreeNode(7,
        TreeNode(6),
        TreeNode(9,
          TreeNode(8),
          TreeNode(10),
        )
      )
    )
    val iterator = new BSTIterator(tree)
    while(iterator.hasNext) {
      println(iterator.next())
    }
  }
}
