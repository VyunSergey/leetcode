package problems.easy

object TicTacToeGame {
  def main(args: Array[String]): Unit = {
    val moves: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    assert(moves.forall(_.length == 2))
    assert(moves.forall(_.forall(List(0, 1, 2).contains(_))))
    val res = ticTacToe(moves)
    println(res)
  }

  def ticTacToe(moves: Array[Array[Int]]): String = {
    var i = 0
    val size = 3
    val board = Array.fill(size)(Array.fill(size)(0))

    moves.foldLeft((Option.empty[String], board)) { case ((wnr, brd), arr) =>
      val Array(row, col) = arr.take(2)
      (wnr orElse {
        brd(row)(col) = if (i % 2 == 0) 1 else -1
        // println(show(brd))
        // println()
        i = (i + 1) % 2
        winner(board)
      }, brd)
    }._1.getOrElse {
      moves.length match {
        case len if len == size * size => "Draw"
        case _ => "Pending"
      }
    }
  }

  def rowWinner(board: Array[Array[Int]], row: Int): Option[String] = {
    val len = board.length
    val (countX, countO) = (0 until len).foldLeft((0, 0)) { case ((countX, countO), i) =>
      (countX + (if (board(row)(i) == 1) 1 else 0),
        countO + (if (board(row)(i) == -1) 1 else 0))
    }
    Some(countX).filter(_ == len).map(_ => "A") orElse
      Some(countO).filter(_ == len).map(_ => "B")
  }

  def columnWinner(board: Array[Array[Int]], col: Int): Option[String] = {
    val len = board.length
    val (countX, countO) = (0 until len).foldLeft((0, 0)) { case ((countX, countO), i) =>
      (countX + (if (board(i)(col) == 1) 1 else 0),
        countO + (if (board(i)(col) == -1) 1 else 0))
    }
    Some(countX).filter(_ == len).map(_ => "A") orElse
      Some(countO).filter(_ == len).map(_ => "B")
  }

  def leftDiagonalWinner(board: Array[Array[Int]]): Option[String] = {
    val len = board.length
    val (countX, countO) = (0 until len).foldLeft((0, 0)) { case ((countX, countO), i) =>
      (countX + (if (board(i)(i) == 1) 1 else 0),
        countO + (if (board(i)(i) == -1) 1 else 0))
    }
    Some(countX).filter(_ == len).map(_ => "A") orElse
      Some(countO).filter(_ == len).map(_ => "B")
  }

  def rightDiagonalWinner(board: Array[Array[Int]]): Option[String] = {
    val len = board.length
    val (countX, countO) = (0 until len).foldLeft((0, 0)) { case ((countX, countO), i) =>
      (countX + (if (board(i)(board.length - 1 - i) == 1) 1 else 0),
        countO + (if (board(i)(board.length - 1 - i) == -1) 1 else 0))
    }
    Some(countX).filter(_ == len).map(_ => "A") orElse
      Some(countO).filter(_ == len).map(_ => "B")
  }

  def winner(board: Array[Array[Int]]): Option[String] = {
    // rows
    board.indices.map(row => rowWinner(board, row)).reduce(_ orElse _) orElse
      // columns
      board.indices.map(col => columnWinner(board, col)).reduce(_ orElse _) orElse
      // left diagonal
      leftDiagonalWinner(board) orElse
      // right diagonal
      rightDiagonalWinner(board)
  }

  def show(board: Array[Array[Int]]): String = {
    board.map(_.map {
      case  1 => "[X]"
      case -1 => "[O]"
      case  _ => "[_]"
    }.mkString("")).mkString("\n")
  }
}
