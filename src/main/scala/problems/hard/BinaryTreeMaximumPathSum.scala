package problems.hard

import problems.common.TreeNode

object BinaryTreeMaximumPathSum {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      left = TreeNode(4,
        left = TreeNode(11,
          left = TreeNode(7),
          right = TreeNode(2,
            left = null,
            right = TreeNode(8)
          )
        ),
        right = null
      ),
      right = TreeNode(8,
        left = TreeNode(13,
          left = TreeNode(4),
          right = null
        ),
        right = TreeNode(4,
          left = null,
          right = TreeNode(1)
        )
      )
    )
    val res = maxPathSum(tree)
    println(res)
  }

  def maxPathSum(root: TreeNode): Int = {
    var max = Int.MinValue

    def pathSum(node: TreeNode): Int = {
      if (node == null) Int.MinValue
      else {
        val left = pathSum(node.left)
        val right = pathSum(node.right)

        max = Math.max(max, node.value + Math.max(left, 0) + Math.max(right, 0))
        // println(s"node=${node.value} left=$left right=$right max=$max")
        node.value + Seq(left, right, 0).max
      }
    }

    pathSum(root)
    max
  }
}
