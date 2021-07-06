package problems.medium

import scala.collection.mutable

object DecodeString {
  def main(args: Array[String]): Unit = {
    val string: String = Console.in.readLine()
    val res: String = decodeString(string)
    println(res)
  }

  def decodeString(s: String): String = {
    var stk = 0
    var str1 = ""
    var str2 = ""
    var str3 = ""
    val arr = mutable.ArrayBuffer.empty[(Int, String)]
    s.foreach { c =>
      if (c == '[') {
        if (stk == 0) {
          stk += 1
          // println("2: " + str2)
          arr += ((-1, str2))
          str2 = ""
        } else {
          stk += 1
          str1 += c
        }
      }
      else if (c == ']') {
        stk -= 1
        if (stk == 0) {
          // println("d: " + str3 + " 1: " + str1)
          arr += ((str3.toInt, str1))
          str1 = ""
          str3 = ""
        } else {
          str1 += c
        }
      }
      else {
        if (stk > 0) str1 += c
        else if (c.isDigit) str3 += c
        else str2 += c
      }
    }
    arr += ((-1, str2))
    // println(arr.toList)
    arr.map { case (i, str) =>
      if (i < 0) str
      else if (str.exists(List('[', ']').contains(_))) decodeString(str) * i
      else str * i
    }.reduce(_ + _)
  }

}
