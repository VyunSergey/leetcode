package problems.easy

import problems.common.TreeNode

object BinaryTreeInorderTraversal {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(1,
      TreeNode(2,
        TreeNode(4, TreeNode(8), TreeNode(9)), TreeNode(5)
      ), TreeNode(3,
        TreeNode(6), TreeNode(7, null, TreeNode(10))
      ))
    val res = inorderTraversal(tree)
    println(res)
  }

  def inorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil
    else inorderTraversal(root.left) ++ List(root.value) ++ inorderTraversal(root.right)
  }
}
