package problems.easy

object FactorialTrailingZeroes {
  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val res = trailingZeroes(n)
    println(res)
  }

  def trailingZeroes(n: Int): Int = {
    var num = n
    var res = 0

    while(num != 0) {
      res += num / 5
      num = num / 5
    }

    res
  }
}
