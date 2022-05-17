package problems.medium

import problems.common.TreeNode

import scala.collection.mutable

object MaximumProductOfSplitBinaryTree {
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
