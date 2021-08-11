package problems.medium

object FlipStringToMonotoneIncreasing {
  def main(args: Array[String]): Unit = {
    val str: String = Console.in.readLine()
    assert(str.forall(List('0', '1').contains))
    val res: Int = minFlipsMonoIncreasing(str)
    println(res)
  }

  def minFlipsMonoIncreasing(str: String): Int = {
    var flipToZero = 0
    var flipToOne = 0
    // each char choose flip to zero of flip to one
    str.foreach { c =>
      // flip to zero
      flipToZero += c - '0'
      // flip to one
      flipToOne = Math.min(flipToZero, flipToOne + 1 - (c - '0'))
    }
    flipToOne
  }
}
