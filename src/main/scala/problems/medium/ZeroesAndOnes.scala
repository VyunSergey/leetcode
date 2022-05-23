package problems.medium

import scala.collection.mutable

object ZeroesAndOnes {
  def main(args: Array[String]): Unit = {
    val Array(m, n) = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }.take(2)
    println(m, n)
    val strs: Array[String] = Console.in.readLine().split(" ")
      .map(_.filter(List('0', '1').contains))
    println(strs.mkString("[", ",", "]"))

    val res = findMaxForm(strs, m, n)
    println(res)
  }

  // m - max zeros count
  // n - max ones count
  def findMaxForm(strs: Array[String], m: Int, n: Int): Int = {
    val len: Int = strs.length
    val cache: mutable.HashMap[Int, Int] = mutable.HashMap.empty[Int, Int]

    def key(m: Int, n: Int, i: Int): Int = {
      1000000 * m + 1000 * n + i
    }

    def zerosOnes(str: String): (Int, Int) = {
      str.foldLeft((0, 0)) { case ((ones, zeros), chr) =>
        (ones + (if (chr == '0') 1 else 0), zeros + (if (chr == '1') 1 else 0))
      }
    }

    val zos: Array[(Int, Int)] = strs.map(zerosOnes)

    def dfs(m: Int, n: Int, i: Int): Int = {
      println((m, n, key(m, n, i), cache.get(key(m, n, i))))
      cache.getOrElseUpdate(key(m, n, i), {
        if (m < 0 || n < 0) Int.MinValue
        else if (i == len) 0
        else {
          val (zeros, ones) = zos(i)
          Math.max(dfs(m, n, i + 1), 1 + dfs(m - zeros, n - ones, i + 1))
        }
      })
    }

    dfs(m, n, 0)
  }
}
