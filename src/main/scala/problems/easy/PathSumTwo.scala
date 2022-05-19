package problems.easy

import problems.common.TreeNode

object PathSumTwo {
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
    val target: Int = Console.in.readLine().toInt
    val res: List[List[Int]] = pathSum(tree, target)
    println(res.map(_.mkString("[", ", ", "]")).mkString("\n"))
  }

  def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] = {
    if (root == null) return List.empty[List[Int]]
    if (root.left == null && root.right == null && root.value == targetSum) return List(List(root.value))

    pathSum(root.left, targetSum - root.value).map(root.value :: _) ++
      pathSum(root.right, targetSum - root.value).map(root.value :: _)
  }
}
