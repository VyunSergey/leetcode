package problems.medium

object PredictTheWinner {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split("\\s+").map(_.toInt)
    val res = predictTheWinner(nums)
    println(s"winner=${if (res) "Player_1" else "Player_2"} nums=${nums.mkString("[", "][", "]")}")
  }

  def predictTheWinner(nums: Array[Int]): Boolean = {
    def pred(turn: Int, sum: Int, left: Int, right: Int): Int = {
      // end
      if (left == right) return sum + turn * nums(left)

      // step
      val sl = pred(-turn, sum + turn * nums(left), left + 1, right)
      val sr = pred(-turn, sum + turn * nums(right), left, right - 1)
      turn * Math.max(turn * sl, turn * sr)
    }

    pred(1, 0, 0, nums.length - 1) >= 0
  }
}
