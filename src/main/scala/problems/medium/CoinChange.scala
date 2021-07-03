package problems.medium

object CoinChange {
  def main(args: Array[String]): Unit = {
    val coins: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val amount: Int = Console.in.readLine().toInt
    val res = coinChange(coins, amount)
    println(res)
  }

  def coinChange(coins: Array[Int], amount: Int): Int = {
    val coins2 = coins.filter(_ <= amount)
    if (amount == 0) return 0
    else if (coins2.length == 0) return -1
    else if (coins2.length == 1) {
      val coin = coins2.head
      if (amount % coin == 0) return amount / coin
      else return -1
    }
    else if (coins2.forall(_ % 2 == 0) && amount % 2 != 0) return -1
    val dp = Array.fill(amount + 1)(-1)
    // println(dp.toList)
    coins2.foreach { i =>
      dp(i) = 1
      // println(s"dp($i) = ${dp(i)}")
    }
    var i = coins2.minOption.getOrElse(-1) + 1
    while(i <= amount) {
      if (dp(i) < 0) {
        dp(i) = coins2.filter(_ < i)
          .map(coin => dp(i - coin))
          .filter(_ > 0)
          .minOption
          .map(_ + 1)
          .getOrElse(-1)
      }
      // println(s"dp($i) = ${dp(i)}")
      i += 1
    }
    // println(dp.toList)
    // i = amount
    // while(dp(i) < 0) i -= 1
    dp(amount)
  }
}
