package problems.easy

import problems.common.TreeNode

object PathSum {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      left = TreeNode(4,
        left = TreeNode(11,
          left = TreeNode(7),
          right = TreeNode(2)
        ),
        right = null
      ),
      right = TreeNode(8,
        left = TreeNode(13),
        right = TreeNode(4,
          left = null,
          right = TreeNode(1)
        )
      )
    )
    val target: Int = Console.in.readLine().toInt
    val res: Boolean = hasPathSum(tree, target)
    println(res)
  }

  def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {
    if (root == null) return false
    if (root.value == targetSum && root.left == null && root.right == null) return true
    hasPathSum(root.left, targetSum - root.value) ||
      hasPathSum(root.right, targetSum - root.value)
  }
}
