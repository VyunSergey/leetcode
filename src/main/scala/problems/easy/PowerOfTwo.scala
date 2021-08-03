package problems.easy

object PowerOfTwo {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res: Boolean = isPowerOfTwo(n)
    println(res)
  }

  def isPowerOfTwo(n: Int): Boolean = {
    (n > 0) && ((n & (n - 1)) == 0)
  }
}
