package problems.medium

import scala.collection.mutable

object WordSearch {
  def main(args: Array[String]): Unit = {
    val board = Array(
      Array('A', 'E', 'D', 'F', 'H'),
      Array('B', 'C', 'G', 'A', 'S'),
      Array('S', 'U', 'J', 'K', 'L'),
      Array('A', 'B', 'C', 'E', 'D')
    )
    val word: String = Console.in.readLine().filter(_.isLetter)
    val res: Boolean = exist(board, word)
    println(res)
  }

  def exist(board: Array[Array[Char]], word: String): Boolean = {
    WordSearch(board).search(word.toCharArray.toList)
  }

  case class WordSearch(private val board: Array[Array[Char]]) {
    def height: Int = board.length
    def width: Int = if (height > 0) board(0).length else 0
    def size: (Int, Int) = (height, width)
    def charAt(i: Int, j: Int): Char = board(i)(j)
    def indicesOf(char: Char): Array[(Int, Int)] = {
      val buffer = mutable.ArrayBuffer.empty[(Int, Int)]
      board.indices.foreach { i =>
        board(i).indices.foreach { j =>
          if (charAt(i, j) == char) buffer += ((i, j))
        }
      }
      buffer.toArray
    }
    def neighbors(i: Int, j: Int): Array[(Int, Int)] = {
      Array(i - 1, i, i + 1)
        .filter(a => 0 <= a && a < height)
        .flatMap { a =>
          Array(j - 1, j, j + 1)
            .filter(b => 0 <= b && b < width)
            .map { b => (a, b) }
        }.filter { case (a, b) =>
        Math.abs(a - i) + Math.abs(b - j) == 1
      }
    }
    def search(chars: List[Char],
               start: (Int, Int) = (-1, -1),
               exclude: List[(Int, Int)] = Nil): Boolean = {
      // println((chars, start, exclude))
      (chars, start) match {
        case (Nil, _) => true
        case (char :: tail, (-1, -1)) =>
          indicesOf(char)
            .filterNot(exclude.contains)
            .exists { case (a, b) => search(tail, (a, b), (a, b) :: exclude) }
        case (char :: tail, (i, j)) =>
          neighbors(i, j)
            .filterNot(exclude.contains)
            .map { case (a, b) => (a, b, charAt(a, b))}
            .filter(_._3 == char)
            .exists { case (a, b, _) => search(tail, (a, b), (a, b) :: exclude) }
      }
    }
  }
}

