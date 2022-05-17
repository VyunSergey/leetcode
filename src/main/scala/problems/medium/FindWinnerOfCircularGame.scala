package problems.medium

object FindWinnerOfCircularGame {
  def main(args: Array[String]): Unit = {
    val Array(n, k) = Console.in.readLine().split(" ").map(_.toInt).take(2)
    val res = findTheWinner(n, k)
    println(res)
  }

  def findTheWinner(n: Int, k: Int): Int = {
    val game = Array.fill(n)(1)
    var start = 0
    var count = 1

    // println((s"start=$start", s"count=$count", game.zipWithIndex.map { case (x, i) => if(i == start) s"[$x]" else s"$x" }.mkString("[", ",", "]")))
    while(game.sum > 1) {
      count = 1
      while(count < k) {
        while(game((start + 1) % n) == 0) start = (start + 1) % n
        start = (start + 1) % n
        count += 1
      }
      game(start) = 0
      while(game((start + 1) % n) == 0) start = (start + 1) % n
      start = (start + 1) % n
      // println((s"start=$start", s"count=$count", game.zipWithIndex.map { case (x, i) => if(i == start) s"[$x]" else s"$x" }.mkString("[", ",", "]")))
    }

    game.indexOf(1) + 1
  }
}
