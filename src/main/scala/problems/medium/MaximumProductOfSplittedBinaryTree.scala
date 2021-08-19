package problems.medium

import scala.collection.mutable

object MaximumProductOfSplittedBinaryTree {

  class TreeNode(_value: Int = 0,
                 _left: TreeNode = null,
                 _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object TreeNode {
    def apply(value: Int): TreeNode = new TreeNode(value)
    def apply(value: Int,
              left: TreeNode,
              right: TreeNode): TreeNode = new TreeNode(value, left, right)
  }

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
    val res = maxProduct(tree)
    println(res)
  }

  def maxProduct(root: TreeNode): Int = {
    if (root == null) return 0

    val mod: Int = (Math.pow(10, 9) + 7).toInt
    val set = mutable.HashSet.empty[BigInt]
    var nodeSum = BigInt(0)
    var maxProduct = BigInt(0)

    def sums(root: TreeNode, set: mutable.HashSet[BigInt]): BigInt = {
      if (root == null) return 0
      nodeSum = BigInt(root.value)
      nodeSum += sums(root.left, set)
      nodeSum += sums(root.right, set)
      set += nodeSum
      nodeSum
    }

    val totalSum = sums(root, set)
    set.foreach { sum =>
      maxProduct = maxProduct.max(sum * (totalSum - sum))
    }

    (maxProduct % mod).toInt
  }
}
