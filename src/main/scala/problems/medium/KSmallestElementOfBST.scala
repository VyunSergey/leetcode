package problems.medium

import scala.annotation.tailrec

object KSmallestElementOfBST {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object TreeNode {
    def apply(value: Int): TreeNode = new TreeNode(value)
    def apply(value: Int, left: TreeNode, right: TreeNode): TreeNode =
      new TreeNode(value, left, right)
  }

  def main(args: Array[String]): Unit = {
    val root: TreeNode = TreeNode(3,
      left = TreeNode(1, TreeNode(0), TreeNode(2)),
      right = TreeNode(4)
    )
    val k: Int = Console.in.readLine().toInt
    val res = kthSmallest(root, k)
    println(res)
  }

  @tailrec
  def kthSmallest(root: TreeNode, k: Int): Int = {
    if (root != null) {
      if (k == 0) {
        // println(s"node=${root.value}, k=$k")
        return root.value
      }
      val left = len(root.left)
      // println(s"node=${root.value}, k=$k, left=$left")
      if (left >= k) kthSmallest(root.left, k)
      else if (left + 1 == k) root.value
      // else if (left == k) max(root.left)
      else {
        val right = len(root.right)
        // println(s"node=${root.value}, k=$k, left=$left, right=$right")
        if (right >= k - left - 1) kthSmallest(root.right, k - left - 1)
        else Int.MaxValue
      }
    } else Int.MaxValue
  }

  @tailrec
  def max(root: TreeNode): Int = {
    if (root != null) {
      if (root.right != null) max(root.right)
      else root.value
    } else Int.MinValue
  }

  def len(root: TreeNode): Int = {
    if (root != null) {
      if (root.left == null && root.right == null) 1
      else {
        len(root.left) + len(root.right) + 1
      }
    } else 0
  }
}
