package problems.easy

import problems.common.TreeNode
import scala.annotation.tailrec

object TwoSumInBST {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      TreeNode(3, TreeNode(2), TreeNode(4)),
      TreeNode(6, null, TreeNode(7))
    )
    val target: Int = Console.in.readLine().toInt
    val res = findTarget(tree, target)
    println(res)
  }

  def findTarget(root: TreeNode, k: Int): Boolean = {
    findTraverse(root, root, k)
  }

  def findTraverse(node: TreeNode, root: TreeNode, k: Int): Boolean = {
    if (node == null) false
    else if (findOne(root, node, k - node.value)) true
    else if (findTraverse(node.left, root, k)) true
    else if (findTraverse(node.right, root, k)) true
    else false
  }

  @tailrec
  def findOne(root: TreeNode, excludeNode: TreeNode, target: Int): Boolean = {
    if (root == null) false
    else if (root != excludeNode && root.value == target) true
    else if (root.value > target) findOne(root.left, excludeNode, target)
    else if (root.value < target) findOne(root.right, excludeNode, target)
    else false
  }
}
