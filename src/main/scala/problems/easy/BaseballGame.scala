package problems.easy

import scala.collection.mutable

object BaseballGame {
  def main(args: Array[String]): Unit = {
    val game: Array[String] = Console.in.readLine().split(" ").map {
      case str if str.toIntOption.isDefined => Some(str)
      case str@"+" => Some(str)
      case str@"D" => Some(str)
      case str@"C" => Some(str)
      case _ => None
    }.collect { case Some(x) => x }
    val res = calPoints(game)
    println(res)
  }

  def calPoints(ops: Array[String]): Int = {
    var i = 0
    var sum = 0
    var temp = 0
    var current = 0
    var previous = 0
    val buf = mutable.ArrayBuffer.empty[Int]

    val pattern = """([-]*\d+)""".r

    ops.foreach {
      case pattern(num) =>
        temp = current
        current = num.toInt
        previous = temp
        sum += current
        buf += current
        i += 1
      case "+" =>
        temp = current
        current = previous + current
        previous = temp
        sum += current
        buf += current
        i += 1
      case "D" =>
        temp = current
        current = 2 * current
        previous = temp
        sum += current
        buf += current
        i += 1
      case "C" =>
        temp = current
        current = previous
        previous = buf.applyOrElse[Int, Int](i - 3, _ => 0)
        sum -= temp
        buf.remove(i - 1)
        i -= 1
    }
    sum
  }
}
