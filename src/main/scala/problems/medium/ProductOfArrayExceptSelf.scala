package problems.medium

object ProductOfArrayExceptSelf {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split(" ").map(_.toInt)
    val res = productExceptSelf(nums)
    println(res.toList)
  }

  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    var i = 0
    val prd = nums.filterNot(_ == 0).product.toDouble
    val zeroInd = nums.indexOf(0)
    val zeroCount = nums.count(_ == 0)
    nums.foreach { n =>
      nums(i) =
        if (zeroCount > 1 || (zeroInd > -1 && zeroInd != i)) 0
        else if (n != 0) (prd / n).toInt
        else prd.toInt
      i += 1
    }
    nums
  }
}
