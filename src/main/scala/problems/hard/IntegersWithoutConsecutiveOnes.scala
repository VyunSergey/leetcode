package problems.hard

object IntegersWithoutConsecutiveOnes {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res: Int = findIntegers(n)
    println(res)
  }

  def findIntegers(n: Int): Int = {
    val fib = new Array[Int](32)

    // number of binary numbers with n bits without consecutive 1's
    // is a fibonacci numbers f(n) = f(n-1) + f(n-2)
    fib(0) = 1
    fib(1) = 2
    (2 until fib.length).foreach { i =>
      fib(i) = fib(i - 1) + fib(i - 2)
    }

    var i = 30
    var sum = 0
    var prevBit = 0
    var break = false

    while (i >= 0 && !break) {
      // i-th bit position is 1
      if ((n & (1 << i)) != 0) {
        // add fib(i) to sum
        sum += fib(i)
        // if find two consecutive 1's then break
        if (prevBit == 1) {
          sum -= 1
          break = true
        }
        // update prevBit
        prevBit = 1
        // i-th bit position is 0
      } else {
        // update prevBit
        prevBit = 0
      }
      i -= 1
    }
    sum + 1
  }
}
