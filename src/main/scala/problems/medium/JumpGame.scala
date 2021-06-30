package problems.medium

object JumpGame {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val res: Boolean = canJump(nums)
    println(res)
  }

  def canJump(nums: Array[Int]): Boolean = {
    var jumped = 0
    nums.zipWithIndex.foreach { case (step, i) =>
      // current position = i
      // can jump with steps: 0, 1, ..., step
      // current max jumped position = jumped
      if (jumped >= i && jumped <= i + step) jumped = i + step
    }
    if (jumped >= nums.length - 1) true else false
  }
}
