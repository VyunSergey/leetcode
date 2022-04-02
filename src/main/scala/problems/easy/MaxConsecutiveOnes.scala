package problems.easy

object MaxConsecutiveOnes {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split(",")
      .map(_.toIntOption).collect { case Some(x) => x }
    assert(nums.nonEmpty)
    assert(nums.forall(List(0, 1).contains))
    val res = findMaxConsecutiveOnes(nums)
    println(res)
  }

  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    var max = 0
    var start = -1

    nums.indices.foreach { i =>
      // println((start, i, nums(i), max))
      if (nums(i) == 1 && start < 0) start = i
      if (nums(i) == 0 && start >= 0) {
        max = Math.max(max, i - start)
        start = -1
      }
    }
    if (start >= 0) {
      max = Math.max(max, nums.length - start)
    }
    max
  }
}
