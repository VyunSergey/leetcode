package problems.easy

import problems.common.TreeNode

object SameTree {
  def main(args: Array[String]): Unit = {
    val tree1 = TreeNode(5,
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
    val tree2 = TreeNode(5,
      left = TreeNode(4,
        left = TreeNode(11,
          left = TreeNode(7),
          right = TreeNode(2,
            left = TreeNode(0),
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
    val res1: Boolean = isSameTree(tree1, tree1)
    println(res1)
    val res2: Boolean = isSameTree(tree1, tree2)
    println(res2)
    val res3: Boolean = isSameTree(tree2, tree2)
    println(res3)
  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p == null || q == null) {
      if (p == null && q == null) true else false
    } else if ((p.left == null && p.right == null) || (q.left == null && q.right == null)) {
      if ((p.left == null && p.right == null) && (q.left == null && q.right == null) && p.value == q.value) true else false
    } else {
      isSameTree(p.left, q.left) && isSameTree(p.right, q.right) && p.value == q.value
    }
  }
}
