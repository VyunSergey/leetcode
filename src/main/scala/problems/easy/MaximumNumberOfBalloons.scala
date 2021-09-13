package problems.easy

import scala.collection.mutable

object MaximumNumberOfBalloons {
  def main(args: Array[String]): Unit = {
    val text = Console.in.readLine()
    val res = maxNumberOfBalloons(text)
    println(res)
  }

  def maxNumberOfBalloons(text: String): Int = {
    val map = mutable.HashMap(
      'b' -> (1, 0), 'a' -> (1, 0), 'l' -> (2, 0), 'o' -> (2, 0), 'n' -> (1, 0)
    )

    text.foreach { chr =>
      if (map.contains(chr)) {
        val (num, cnt) = map(chr)
        map += (chr -> (num, cnt + 1))
      }
    }

    map.foldLeft(Option.empty[Int]) { case (acc, (_, (num, cnt))) =>
      acc.map(a => Math.min(a, cnt / num)).orElse(Some(cnt / num))
    }.getOrElse(0)
  }
}
