package study.array

object MaximumProductSubArray {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }
    println(nums.toList)

    val res: Int = maxProduct(nums)
    println(res)
  }

  // TODO implement solution
  def maxProduct(nums: Array[Int]): Int = {
    ???
  }
}
