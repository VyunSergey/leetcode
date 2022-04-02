package problems.easy

object BinaryTreePostorderTraversal {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(1,
      TreeNode(2,
        TreeNode(4, TreeNode(8), TreeNode(9)), TreeNode(5)
      ), TreeNode(3,
        TreeNode(6), TreeNode(7, null, TreeNode(10))
      ))
    val res = postorderTraversal(tree)
    println(res)
  }

  def postorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil
    else postorderTraversal(root.left) ++ postorderTraversal(root.right) ++ List(root.value)
  }
}
