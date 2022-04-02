package problems.hard

object ExpressionAddOperators {
  def main(args: Array[String]): Unit = {
    val num = Console.in.readLine()
    val target = Console.in.readLine().toInt
    assert(num.nonEmpty)
    assert(num.forall(_.isDigit))
    val res = addOperators(num, target)
    println(res.length)
    res.take(100).foreach(println)
  }

  def addOperators(num: String, target: Int): List[String] = {
    addOps(num, target, "", 1)
  }

  def addOps(num: String, target: Int, digits: String, mult: Int): List[String] = {
    if (num.length == 1) {
      val last = int(num.last)
      val elem = int(last, digits)
      if (elem >= 0 && target == elem * mult) List("" + last) else Nil
    } else {
      val init = num.init
      val last = int(num.last)
      val elem = int(last, digits)
      // +
      (if (elem >= 0) addOps(init, target - elem * mult, "", 1).map(_ + "+" + last) else Nil) ++
        // -
        (if (elem >= 0) addOps(init, target + elem * mult, "", 1).map(_ + "-" + last) else Nil) ++
        // *
        (if (elem >= 0) addOps(init, target, "", elem * mult).map(_ + "*" + last) else Nil) ++
        // digit
        addOps(init, target, last + digits, mult).map(_ + "" + last)
    }
  }

  def int(c: Char): Int = c - '0'

  def int(i: Int, digits: String): Int = {
    if (i == 0 && digits.nonEmpty) -1 else ("" + i + digits).toIntOption.getOrElse(-1)
  }
}
