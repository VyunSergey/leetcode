package problems.easy

object MoveZeroes {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }
    moveZeroes(nums)
    println(nums.mkString("[", ", ", "]"))
  }

  def moveZeroes(nums: Array[Int]): Unit = {
    var i = 0
    val (zeros, nonZeros) = nums.partition(_ == 0)

    nonZeros.foreach { a =>
      nums(i) = a
      i += 1
    }

    zeros.foreach { a =>
      nums(i) = a
      i += 1
    }
  }
}
