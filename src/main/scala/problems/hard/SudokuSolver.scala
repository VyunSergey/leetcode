package problems.hard

object SudokuSolver {

  case class SudokuArray(arr: Array[Char]) {
    private val allDigits: Set[Int] = {
      Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
    def show: String = arr.mkString("[", "][", "]")
    def toDigits: Array[Option[Int]] = {
      arr.map(c => c - '0').map {
        case a if allDigits.contains(a) => Some(a)
        case _ => None
      }
    }
    def solvedDigits: Set[Int] = {
      toDigits.collect {
        case Some(a) => a
      }.toSet
    }
    def size: Int = solvedDigits.size
    def unsolvedDigits: Set[Int] = {
      allDigits.diff(solvedDigits)
    }
    def isValid: Boolean = {
      solvedDigits.size == arr.filterNot(_ == '.').length
    }
  }

  case class SudokuBoard(board: Array[Array[Char]]) {
    def rows: Array[SudokuArray] = {
      board.map(SudokuArray.apply)
    }
    def columns: Array[SudokuArray] = {
      board.transpose.map(SudokuArray.apply)
    }
    def squares: Array[SudokuArray] = {
      (for {
        i <- Range(0, 6, 3).inclusive
        j <- Range(0, 6, 3).inclusive
      } yield {
        SudokuArray((for {
          k <- 0 until 9
        } yield {
          board(i+k/3)(j+k%3)
        }).toArray)
      }).toArray
    }
    def showR: String = {
      rows.map(_.show).mkString("\n")
    }
    def showC: String = {
      columns.map(_.show).mkString("\n")
    }
    def showS: String = {
      squares.map(_.show).mkString("\n")
    }
    def toDigits: Array[Array[Option[Int]]] = {
      rows.map(_.toDigits)
    }
    def allDigits: Map[(Int, Int), Option[Int]] = {
      toDigits.map(_.zipWithIndex).zipWithIndex
        .flatMap { case (arr, j) =>
          arr.map { case (a, i) =>
            ((i, j), a)
          }
        }.toMap
    }
    def unsolvedDigits(i: Int, j: Int): Set[Int] = {
      val rd = rows(j).unsolvedDigits
      val cd = columns(i).unsolvedDigits
      val sd = squares(i/3 + 3*(j/3)).unsolvedDigits
      rd & cd & sd
    }
    def allUnsolvedDigits: Map[(Int, Int), Set[Int]] = {
      allDigits.collect { case ((i, j), None) =>
        ((i, j), unsolvedDigits(i, j))
      }
    }
    def isValid(i: Int, j: Int): Boolean = {
      rows(j).isValid &&
        columns(i).isValid &&
        squares(i/3 + 3*(j/3)).isValid
    }
    def isValid: Boolean = {
      allDigits.keys.forall { case (i, j) =>
        isValid(i, j)
      }
    }
    def isSolved: Boolean = {
      allUnsolvedDigits.isEmpty
    }
    def update(i: Int, j: Int, c: Char): SudokuBoard = {
      SudokuBoard(board.updated(j, board(j).updated(i, c)))
    }
    def updateAll(values: Map[(Int, Int), Char]): SudokuBoard = {
      values.foldLeft(this) { case (sudoku, ((i, j), c)) =>
        //println(((i, j), c))
        sudoku.update(i, j, c)
      }
    }
    def solve: Set[SudokuBoard] = {
      if (isSolved) Set(this)
      else {
        val oneDigit = allUnsolvedDigits
          .filter(_._2.size == 1)
        val moreDigits = allUnsolvedDigits
          .filter(_._2.size > 1)
          .minByOption(_._2.size)

        if (oneDigit.nonEmpty) {
          Set(updateAll(oneDigit.view.mapValues {digits =>
            (digits.head + '0').toChar
          }.toMap))
        }
        else if (moreDigits.isDefined) {
          val ((i, j), digits) = moreDigits.get
          digits.map { a =>
            update(i, j, (a + '0').toChar)
          }
        }
        else {
          Set.empty[SudokuBoard]
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val board: Array[Array[Char]] = Array.fill(9)(Array.fill(9)('.'))

    for {
      i <- 0 to 8
    } yield {
      val arr: Array[Char] = Console.in.readLine.split("\\s")
        .map(_.toIntOption)
        .collect {
          case Some(a) => (a + '0').toChar
          case None => '.'
        }
        .take(9)
      board.update(i, arr)
    }

    val sudoku = SudokuBoard(board)
    println(sudoku.showR)
    println(s"Valid Sudoku: ${sudoku.isValid}")
    println()

    solveSudoku(board)

    val solvedSudoku = SudokuBoard(board)
    println(solvedSudoku.showR)
    println(s"Valid Sudoku: ${solvedSudoku.isValid}")
    println()
  }

  def solveSudoku(board: Array[Array[Char]]): Unit = {
    var sudoku = SudokuBoard(board)
    //println(sudoku.showR)
    //println()

    var solutions = Set(sudoku)
    //sudoku.allUnsolvedDigits.foreach(println)
    //println()

    while(!solutions.exists(_.isSolved)) {
      solutions = solutions.flatMap(_.solve)
      /*
      solutions.foreach { sudoku =>
        println(sudoku.showR)
        println()
      }
       */
      //println("-" * 30)
    }
    sudoku = solutions.find(_.isSolved).get
    //sudoku.allUnsolvedDigits.foreach(println)
    //println()

    sudoku.allDigits.foreach {
      case ((i, j), Some(a)) =>
        board.update(j, board(j).updated(i, (a + '0').toChar))
      case _ => ()
    }
  }
}
