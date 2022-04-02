package study.array

object CountPrimes {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res: Int = countPrimes(n)
    println(res)
  }

  def countPrimes(n: Int): Int = {
    var i = 2
    var j = 0
    val isPrime = Array.fill(n)(1)
    // 2 - is prime -> [4, 6, ...] not prime
    // 3 - is prime -> [6, 9, ...] not prime
    // ...
    if (n < 3) return 0
    isPrime(0) = 0
    isPrime(1) = 0

    while(i < Math.sqrt(n)) {
      if (isPrime(i) == 1) {
        j = 2
        while(i * j < n) {
          isPrime(i * j) = 0
          j += 1
        }
      }
      // println((i, isPrime(i)))
      i += 1
    }

    // println(isPrime.zipWithIndex.mkString("[", ", ", "]"))
    isPrime.sum
  }
}
