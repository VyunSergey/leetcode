package problems.medium

import problems.common.TreeNode

import scala.annotation.tailrec
import scala.collection.mutable

object PathSumTwo {
  def main(args: Array[String]): Unit = {
    val tree = TreeNode(5,
      TreeNode(4,
        TreeNode(11,
          TreeNode(7),
          TreeNode(2),
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

    val targetSum = Console.in.readLine().toInt
    val res = pathSum(tree, targetSum)
    println(res)
  }

  def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] = {
    val buffer: mutable.ListBuffer[List[Int]] = mutable.ListBuffer.empty[List[Int]]

    @tailrec
    def loop(candidates: List[(TreeNode, Int, List[Int])]): Unit = {
      // println(candidates.map { case (node, target, values) => (if (node != null) node.value else -1, target, values) })
      candidates match {
        case Nil =>
        case head :: tail =>
          val (node, targetSum, values) = head
          if (node == null) {
            loop(tail)
          } else if (node.value == targetSum && node.left == null && node.right == null) {
            // println(s"RESULT + ${(if (node != null) node.value else -1, targetSum, values)}")
            buffer += (node.value :: values).reverse
            loop(tail)
          } else {
            val left = (node.left, targetSum - node.value, node.value :: values)
            val right = (node.right, targetSum - node.value, node.value :: values)
            loop(left :: right :: tail)
          }
      }
    }

    loop(List((root, targetSum, Nil)))
    buffer.result
  }
}
