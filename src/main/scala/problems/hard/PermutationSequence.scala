package problems.hard

import scala.annotation.tailrec

object PermutationSequence {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val k: Int = Console.in.readLine().toInt
    assert( 0 < k && BigInt(k) < factorial(n))
    val res: String = getPermutation(n, k)
    println(res)
  }

  // [1, 2, ..., n] - n! permutations
  // "1..." - [0*(n-1)!+1; 1*(n-1)!] = (n-1)! permutations
  // "2..." - [1*(n-1)!+1; 2*(n-1)!] = (n-1)! permutations
  // ...
  // "n..." - [(n-1)*(n-1)!+1; n*(n-1)!] = (n-1)! permutations
  def getPermutation(n: Int, k: Int): String = {
    permutation(n, k, List.range(1, n + 1))
  }

  def factorial(n: Int): BigInt = (BigInt(1) to BigInt(n)).product

  @tailrec
  def permutation(n: Int, k: Int, digits: List[Int], res: String = ""): String = {
    // println(s"n=$n k=$k digits=${digits.mkString("[", ", ", "]")} res=$res")
    if (k == 0) res
    else if (n == 1) res + digits.head.toString
    else {
      var i = 1

      while(i <= n && BigInt(k) > BigInt(i) * factorial(n - 1)) i += 1

      if (i == n && BigInt(k) == BigInt(i) * factorial(n - 1)) {
        res + digits.reverse.mkString
      } else permutation(
        n - 1,
        (BigInt(k) - BigInt(i - 1) * factorial(n - 1)).toInt,
        digits.take(i - 1) ++ digits.drop(i),
        res + digits(i - 1).toString
      )
    }
  }
}
