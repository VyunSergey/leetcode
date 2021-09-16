package problems.hard

object NQueens {
  val queen = "Q"
  val dot = "."

  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res = solveNQueens(n)
    println(res.map(_.map(s => s"[$s]").mkString("\n")).mkString("\n\n"))
  }

  def show(n: Int, queens: List[(Int, Int)]): List[String] = {
    List.range(0, n).map { i =>
      List.range(0, n).map { j =>
        if (queens.contains((i, j))) queen else dot
      }.mkString
    }
  }

  def solveNQueens(n: Int): List[List[String]] = {
    queens(n).map(queen => show(n, queen))
  }

  def queens(n: Int): List[List[(Int, Int)]] = {
    def inner(k: Int): List[List[(Int, Int)]] = {
      if (k == 0) List(List.empty[(Int, Int)])
      else {
        for {
          queens <- inner(k - 1)
          column <- 0 until n
          // _ = println(s"k=$k queens=$queens col=$column queen=${(k - 1, column)} isSafe=${isSafe((k - 1, column), queens)}")
          queen = (k - 1, column) if isSafe(queen, queens)
        } yield queen :: queens
      }
    }
    inner(n)
  }

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]): Boolean = {
    !queens.exists(q => inCheck(queen, q))
  }

  def inCheck(q1: (Int, Int), q2: (Int, Int)): Boolean = {
    // row
    q1._1 == q2._1 ||
      // column
      q1._2 == q2._2 ||
      // diagonal
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs
  }
}
