package problems.easy

import problems.common.TreeNode

object ConvertSortedArrayToBinarySearchTree {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val res: TreeNode = sortedArrayToBST(nums)
    println(printTree(res))
  }

  def printTree(node: TreeNode): String = {
    if (node == null) {
      "TreeNode()"
    } else if (node.left == null && node.right == null) {
      s"TreeNode(v=${node.value})"
    } else {
      s"TreeNode(v=${node.value} l=${printTree(node.left)} r=${printTree(node.right)})"
    }
  }

  def sortedArrayToBST(nums: Array[Int]): TreeNode = {
    val len = nums.length
    if (len > 1) {
      val mid = len / 2
      new TreeNode(
        nums(mid),
        sortedArrayToBST(nums.take(mid)),
        sortedArrayToBST(nums.drop(mid + 1))
      )
    } else if (len == 1) {
      new TreeNode(nums.head)
    } else null
  }

}
