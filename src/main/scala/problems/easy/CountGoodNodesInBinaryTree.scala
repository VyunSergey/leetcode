package problems.easy

object CountGoodNodesInBinaryTree {

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
    val res = goodNodes(tree)
    println(res)
  }

  def goodNodes(root: TreeNode): Int = {
    if (root == null) return 0
    nodes(root, root.value)
  }

  def nodes(root: TreeNode, x: Int): Int = {
    if (root == null) return 0

    (if (root.value < x) 0 else 1) +
      nodes(root.left, Math.max(x, root.value)) +
      nodes(root.right, Math.max(x, root.value))
  }
}
