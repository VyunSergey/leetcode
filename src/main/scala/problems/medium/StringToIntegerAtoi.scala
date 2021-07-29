package problems.medium

object StringToIntegerAtoi {
  def main(args: Array[String]): Unit = {
    val str: String = Console.in.readLine()
    val res: Int = myAtoi(str)
    println(res)
  }

  def myAtoi(s: String): Int = {
    var isNegative = false
    var onlyDigits = false
    var break = false
    var i = 0
    var j = -1

    while(i < s.length && s(i) == ' ') i += 1
    while(i < s.length && !break) {
      // info(s, i)
      if (s(i) == '-' || s(i) == '+') {
        if (onlyDigits) {
          if (j < 0) return 0
          else {
            i -= 1
            break = true
          }
        }
        onlyDigits = true
        if (s(i) == '-') isNegative = true
      } else if (s(i).isDigit) {
        if (j < 0) j = i
        onlyDigits = true
      } else {
        if (j < 0) return 0
        else {
          i -= 1
          break = true
        }
      }
      i += 1
    }
    if (j < 0) return 0
    val str = if (isNegative) "-" + s.slice(j, i) else s.slice(j, i)
    // println(str)
    if (BigInt(str) > BigInt(Int.MaxValue)) Int.MaxValue
    else if (BigInt(str) < BigInt(Int.MinValue)) Int.MinValue
    else BigInt(str).toInt
  }

  def info(s: String, i: Int): Unit = {
    val str = s.toCharArray.zipWithIndex
      .map { case (c, j) =>
        if (i == j) s"[$c]" else s"$c"
      }
    println(str .mkString("|", "", "|"))
  }
}
