package problems.medium

import problems.common.TreeNode
import problems.easy.BinaryTreeInorderTraversal

import scala.annotation.tailrec

object DeleteNodeInBST {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      TreeNode(3,
        TreeNode(2),
        TreeNode(4)
      ),
      TreeNode(6,
        null,
        TreeNode(7),
      )
    )

    println(BinaryTreeInorderTraversal.inorderTraversal(tree))
    val key = Console.in.readLine().toInt
    val newTree = deleteNode(tree, key)
    println(BinaryTreeInorderTraversal.inorderTraversal(newTree))
  }

  @tailrec
  def addLeft(target: TreeNode, source: TreeNode): Unit = {
    if (target != null) {
      if (target.left != null) {
        addLeft(target.left, source)
      } else {
        target.left = source
      }
    }
  }

  @tailrec
  def addRight(target: TreeNode, source: TreeNode): Unit = {
    if (target != null) {
      if (target.right != null) {
        addRight(target.right, source)
      } else {
        target.right = source
      }
    }
  }

  def deleteNode(root: TreeNode, key: Int): TreeNode = {
    @tailrec
    def del(root: TreeNode, prev: TreeNode, sign: Char, curr: TreeNode, key: Int): TreeNode = {
      if (curr != null) {
        if (curr.value < key) {
          del(root, curr, 'R', curr.right, key)
        } else if (curr.value > key) {
          del(root, curr, 'L', curr.left, key)
        } else {
          println(('L', if (curr.left != null) curr.left.value else -1, 'R', if (curr.right != null) curr.right.value else -1))
          if (curr.right != null) {
            addLeft(curr.right, curr.left)
            if (sign == 'L') {
              prev.left = curr.right
            } else if (sign == 'R' ) {
              prev.right = curr.right
            } else {
              return curr.right
            }
          } else if (curr.left != null) {
            addRight(curr.left, curr.right)
            if (sign == 'L') {
              prev.left = curr.left
            } else if (sign == 'R' ) {
              prev.right = curr.left
            } else {
              return curr.left
            }
          } else {
            if (sign == 'L') {
              prev.left = null
            } else if (sign == 'R' ) {
              prev.right = null
            } else {
              return null
            }
          }
          root
        }
      } else root
    }

    del(root, null, '@', root, key)
  }
}
