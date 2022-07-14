package problems.medium

import scala.collection.mutable

object DecodeString {
  def main(args: Array[String]): Unit = {
    val string: String = Console.in.readLine()
    val res: String = decodeString(string)
    println(res)
  }

  def decodeString(s: String): String = {
    val n = s.length
    val stack = mutable.Stack.empty[(String, Int)]
    var brackets = 0
    var i = 0
    var cur = ""
    var res = ""

    while(i < n) {
      // println(s"i=$i s($i)=${s(i)} stack=$stack")
      if (s(i).isDigit) {
        val sb = new mutable.StringBuilder()
        while(i < n && s(i).isDigit) {
          sb += s(i)
          i += 1
        }
        stack.push((cur, sb.result.toInt))
        cur = ""
      } else if (s(i) == '[') {
        brackets += 1
        i += 1
      } else if (s(i).isLetter) {
        val sb = new mutable.StringBuilder()
        while(i < n && s(i).isLetter) {
          sb += s(i)
          i += 1
        }
        cur += sb.result
      } else if (s(i) == ']') {
        val (str, num) = stack.pop()
        cur = str + cur * num
        brackets -= 1
        if (brackets == 0) {
          res += cur
          cur = ""
          // println(s"res=$res")
        }
        i += 1
      }
    }
    // println(s"res=$res cur=$cur brackets=$brackets stack=$stack")

    res + cur
  }

  def decodeString2(s: String): String = {
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
      else if (str.exists(List('[', ']').contains(_))) decodeString2(str) * i
      else str * i
    }.reduce(_ + _)
  }

}
