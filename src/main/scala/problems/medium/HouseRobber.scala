package problems.medium

object HouseRobber {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val res: Int = rob(nums)
    println(res)
  }

  def rob(nums: Array[Int]): Int = {
    var (a, b) = (0, 0)

    nums.indices.foreach { i =>
      if (i % 2 == 0) a = Math.max(a + nums(i), b)
      else b = Math.max(a, b + nums(i))
    }

    Math.max(a, b)
  }
}
