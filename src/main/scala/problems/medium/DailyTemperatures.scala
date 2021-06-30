package problems.medium

import scala.collection.mutable

object DailyTemperatures {
  def main(args: Array[String]): Unit = {
    val temp = Console.in.readLine().split(" ").map(_.toInt)
    val res = dailyTemperatures(temp)
    println(res.toList)
  }

  def dailyTemperatures(temp: Array[Int]): Array[Int] = {
    val days = mutable.ArrayBuffer.empty[Int]
    var i = 0
    temp.foreach { t =>
      val ind = temp.indexWhere(_ > t, i + 1)
      days += (if (ind > 0) ind - i else 0)
      i += 1
    }
    days.toArray
  }
}
