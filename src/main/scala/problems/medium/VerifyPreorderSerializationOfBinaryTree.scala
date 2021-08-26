package problems.medium

object VerifyPreorderSerializationOfBinaryTree {

  def main(args: Array[String]): Unit = {
    val preorder = Console.in.readLine()
    assert(preorder.split(",").forall(str => str.toIntOption.nonEmpty || str == "#"))
    val res = isValidSerialization(preorder)
    println(res)
  }

  def isValidSerialization(preorder: String): Boolean = {
    var diff = 1

    preorder.split(",").foreach { node =>
      diff -= 1
      if (diff < 0) return false
      if (node != "#") diff += 2
    }

    diff == 0
  }

}
