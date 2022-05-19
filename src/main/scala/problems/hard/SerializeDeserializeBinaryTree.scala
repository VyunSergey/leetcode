package problems.hard

import problems.common.TreeNode
import problems.easy.SameTree

object SerializeDeserializeBinaryTree {

  class Codec {
    // Encodes a list of strings to a single string.
    def serialize(root: TreeNode): String = {
      if (root == null) ""
      else s"[${root.value}](${serialize(root.left)},${serialize(root.right)})"
    }

    // Decodes a single string to a list of strings.
    def deserialize(data: String): TreeNode = {
      val pattern1 = """^\[([+-]?\d+)]$""".r
      val pattern2 = """^\[([+-]?\d+)]\((.*)\)$""".r
      data match {
        case "" => null
        case pattern1(value) => TreeNode(value.toInt)
        case pattern2(value, next) =>
          var count = 0
          var i = 0
          while(i < next.length && count >= 0) {
            val c = next(i)
            if (c == '(') count += 1
            else if (c == ')' && count > 0) count -= 1
            else if (c == ',' && count == 0) count = -1
            i += 1
          }
          val (left, right) = (next.take(i - 1), next.drop(i))
          // println(s"l=$left r=$right")
          new TreeNode(value.toInt, deserialize(left), deserialize(right))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: TreeNode =
      TreeNode(0,
        TreeNode(1,
          TreeNode(3,
            TreeNode(-7, null, null), null),
          TreeNode(4,
            null, TreeNode(8, TreeNode(-11, null, null), null))
        ),
        TreeNode(2,
          TreeNode(5,
            TreeNode(-9, null, null), null),
          TreeNode(6,
            null, TreeNode(-10, null, TreeNode(12, null, null)))
        )
      )
    val codec = new Codec
    val encoded: String = codec.serialize(tree)
    println(encoded)
    val decoded: TreeNode = codec.deserialize(encoded)
    println(SameTree.isSameTree(tree, decoded))
  }
}
