package problems.hard

object NMagicalNumber {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val a: Int = Console.in.readLine().toInt
    val b: Int = Console.in.readLine().toInt
    val res: Int = nthMagicalNumber(n, a, b)
    println(res)
  }

  // a, b
  // c = gcd(a, b)
  // l = lcm(a, b) = a * b / gcd(a, b) = a * b / c
  // m - number of Magic Numbers less or equals to l and l is m-th Magic Number
  // m = (l / a) Magic Numbers like i * a, i = [1,...,l / a] including b * a
  //     +
  //     (l / b) Magic Numbers like j * b, j = [1,...,l / b] including a * b
  //     - 1 removing second a * b
  // n = q * m + r
  // q = n / m
  // r = n % m
  // then n-th Magic Number is q * l + r-th Magic Number from [q * l + 1,q * l + 2,...]
  // 1-th Magic Number is either a | b
  // if a <= b then 2-nd Magic Number is either 2 * a | b
  // if b <= 2 * a then 3-rd Magic Number is either 2 * a | 2 * b
  def nthMagicalNumber(n: Int, a: Int, b: Int): Int = {
    val mod: Int = 1000000007
    val c = BigInt(a).gcd(b).toInt
    val l = a / c * b
    val m = l / a + l / b - 1
    val q = n / m
    val r = n % m
    val res = ((q.toLong * l) % mod).toInt

    if (r == 0) return res

    var left = a
    var right = b
    (0 until r - 1).foreach { _ =>
      if (left <= right) left += a
      else right += b
    }

    ((res.toLong + Math.min(left, right)) % mod).toInt
  }
}
