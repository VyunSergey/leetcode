package study.array

/** Given an integer array nums and an integer val, remove all occurrences of val in nums in-place.
 * The relative order of the elements may be changed.
 * Since it is impossible to change the length of the array in some languages,
 * you must instead have the result be placed in the first part of the array nums.
 * More formally, if there are k elements after removing the duplicates,
 * then the first k elements of nums should hold the final result.
 * It does not matter what you leave beyond the first k elements.
 */
object RemoveElement {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }
    val `val`: Int = Console.in.readLine().toInt

    val res: Int = removeElement(nums, `val`)
    println(res)
  }

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    var i = 0
    var move = 0

    while (i < nums.length) {
      // println((i, move, nums.mkString("[", ", ", "]")))
      nums(i - move) = nums(i)
      if (nums(i) == `val`) move += 1
      i += 1
    }

    nums.length - move
  }
}
