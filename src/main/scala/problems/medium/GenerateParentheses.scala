package problems.medium

object GenerateParentheses {
  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine.toInt
    Console.out.println(generateParenthesis(n).mkString("[", ",", "]"))
  }

  def generateParenthesis(n: Int): List[String] = {
    def combine(s1: Set[String], s2: Set[String]): Set[String] = {
      for {
        a <- s1
        b <- s2
      } yield a + b
    }

    def gen(n: Int): Set[String] = {
      if (n == 0) Set("")
      else if (n == 1) Set("()")
      else if (n == 2) Set("()()", "(())")
      else if (n == 3) Set("()()()", "()(())", "(())()", "(()())", "((()))")
      else {
        (for {
          k <- 0 until n
        } yield {
          val xn = gen(n - 1 - k)
          val xk = gen(k)
          Set(
            combine(xn.map("()" + _), xk),
            combine(xn.map("(" + _ + ")"), xk),
            combine(xn.map(_ + "()"), xk),
            combine(xn, xk).map("(" + _ + ")"),
            combine(xn, xk.map("()" + _)),
            combine(xn, xk.map("(" + _ + ")")),
            combine(xn, xk.map(_ + "()"))
          ).flatten
        }).toSet.flatten
      }
    }

    gen(n).toList.sorted
  }
}
