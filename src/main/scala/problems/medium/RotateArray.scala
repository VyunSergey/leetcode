package problems.medium

object RotateArray {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val k: Int = Console.in.readLine().toInt
    rotate(nums, k)
    println(nums.toList)
  }

  def rotate(nums: Array[Int], k: Int): Unit = {
    val len = nums.length
    var i = 0
    val j = k % len
    val rest = nums.takeRight(j)
    // println(rest.toList)
    while (i < len - j) {
      nums(len - i - 1) = nums(len - i - 1 - j)
      i += 1
    }
    // println(nums.toList)
    i = 0
    while (i < j) {
      nums(i) = rest(i)
      i += 1
    }
  }
}
