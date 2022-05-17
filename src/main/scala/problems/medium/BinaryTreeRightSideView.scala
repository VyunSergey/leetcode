package problems.medium

object BinaryTreeRightSideView {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      TreeNode(4,
        TreeNode(11,
          TreeNode(7),
          TreeNode(2,
            TreeNode(21),
            TreeNode(23,
              TreeNode(33),
              TreeNode(35),
            ),
          ),
        ),
        null
      ),
      TreeNode(8,
        TreeNode(13),
        TreeNode(4,
          TreeNode(5),
          TreeNode(1),
        )
      )
    )

    val res = rightSideView(tree)
    println(res)
  }

  def rightSideView(root: TreeNode): List[Int] = {
    if (root != null) {
      val left = rightSideView(root.left)
      val right = rightSideView(root.right)
      root.value :: right ++ left.takeRight(left.length - right.length)
    } else Nil
  }
}
