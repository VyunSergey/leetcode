package problems.medium

import scala.collection.mutable

object BeautifulArray {
  val cache = mutable.HashMap.empty[Int, Array[Int]]

  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res: Array[Int] = beautifulArray(n)
    println(res.mkString("[", ", ", "]"))
  }

  def beautifulArray(n: Int): Array[Int] = {
    func(n)
  }

  def func(n: Int): Array[Int] = {
    if (cache.contains(n)) return cache(n)

    val ans = mutable.ArrayBuffer.empty[Int]

    if (n == 1) {
      ans += 1
    } else {
      // odds
      ans ++= func((n + 1) / 2).map(x => 2 * x - 1)
      // evens
      ans ++= func(n / 2).map(x => 2 * x)
    }
    cache.update(n, ans.toArray)
    ans.toArray
  }
}
