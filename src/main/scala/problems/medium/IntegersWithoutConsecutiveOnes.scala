package problems.medium

object IntegersWithoutConsecutiveOnes {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res = findIntegers(n)
    println(res)
  }

  // n = 1 ["0","1"] f = 2
  // n = 2 ["00","01","10"] f = 3
  // n = 3 ["000","001","010","100","101"] f = 5
  // n = 4 ["0000","0001","0010","0100","0101","1000","1001","1010"] f = 8
  def findIntegers(n: Int): Int = {
    val fib = new Array[Int](32)

    // number of binary numbers with n bits without consecutive 1's
    // is a fibonacci numbers
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
