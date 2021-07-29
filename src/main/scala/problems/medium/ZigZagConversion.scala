package problems.medium

import scala.collection.mutable

object ZigZagConversion {
  def main(args: Array[String]): Unit = {
    val string: String = Console.in.readLine()
    val n: Int = Console.in.readLine().toInt
    val res = convert(string, n)
    println(res)
  }

  def convert(s: String, numRows: Int): String = {
    var i = 0
    var j = 0
    var k = 0
    var forward = true
    val buffers = mutable.HashMap.empty[Int, mutable.ArrayBuffer[Char]]
    if (numRows == 1) return s

    (0 until numRows).foreach { k =>
      buffers += (k -> mutable.ArrayBuffer.empty[Char])
    }

    while(i < s.length) {
      if(forward) {
        buffers(j) += s(i)
        j = (j + 1) % numRows
        if (j == 0 && numRows > 2) forward = false
      } else {
        buffers(numRows - 2 - k) += s(i)
        k = (k + 1) % (numRows - 2)
        if (k == 0) forward = true
      }
      i += 1
    }

    buffers.values
      .map(_.mkString)
      .reduce(_ + _)
  }
}
