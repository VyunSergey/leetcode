package problems.easy

object BinaryTreePreorderTraversal {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(1,
      TreeNode(2,
        TreeNode(4, TreeNode(8), TreeNode(9)), TreeNode(5)
      ), TreeNode(3,
        TreeNode(6), TreeNode(7, null, TreeNode(10))
      ))
    val res = preorderTraversal(tree)
    println(res)
  }

  def preorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil
    else List(root.value) ++ preorderTraversal(root.left) ++ preorderTraversal(root.right)
  }
}
