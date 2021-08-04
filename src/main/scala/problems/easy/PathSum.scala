package problems.easy

object PathSum {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object TreeNode {
    def apply(): TreeNode = new TreeNode()
    def apply(value: Int): TreeNode = new TreeNode(value)
    def apply(value: Int, left: TreeNode, right: TreeNode): TreeNode = new TreeNode(value, left, right)
  }

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
