package problems.easy

object RangeAdditionTwo {
  def main(args: Array[String]): Unit = {
    val m = Console.in.readLine().toInt
    val n = Console.in.readLine().toInt
    val ops: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toInt))
    val res = maxCount(m, n, ops)
    println(res)
  }

  def maxCount(m: Int, n: Int, ops: Array[Array[Int]]): Int = {
    val minX = ops.minByOption(_.head).map(_.head).getOrElse(m)
    val minY = ops.minByOption(_.last).map(_.last).getOrElse(n)

    minX * minY
  }
}
