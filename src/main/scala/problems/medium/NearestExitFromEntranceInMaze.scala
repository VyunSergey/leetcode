package problems.medium

import scala.collection.mutable

object NearestExitFromEntranceInMaze {
  private val directions: List[(Int, Int)] =
    List((-1, 0), (1, 0), (0, -1), (0, 1))

  def main(args: Array[String]): Unit = {
    val entrance: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }.take(2)
    println(entrance.mkString(" "))

    val maze: Array[Array[Char]] = Console.in.readLine().split(" ")
      .map(_.toCharArray.filter(List('.', '+').contains))
    printArr(entrance(0), entrance(1), List((entrance(0), entrance(1))), maze)

    val res = nearestExit(maze, entrance)
    println(s"res=$res")
  }

  def printArr(k: Int, l: Int, path: List[(Int, Int)], arr: Array[Array[Char]]): Unit = {
    val m = arr.length
    val n = arr.head.length
    var str = ""

    (0 until m).foreach { i =>
      (0 until n).foreach { j =>
        if (i == k && j == l) str ++= "@"
        else if (path.contains((i, j))) str ++= "#"
        else str ++= s"${arr(i)(j)}"
      }
      str += "\n"
    }

    println(str)
    println()
  }

  def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int = {
    val m = maze.length
    val n = maze.head.length
    val queue = mutable.Queue.empty[(Int, Int, Int, List[(Int, Int)])]
    val visited = Array.fill(m)(Array.fill(n)(false))
    val Array(startI, startJ) = entrance

    def isValid(i: Int, j: Int): Boolean = {
      0 <= i && i < m &&
      0 <= j && j < n &&
      maze(i)(j) == '.' &&
      !visited(i)(j)
    }

    def isExit(i: Int, j: Int): Boolean = {
      (i == 0 || i == m - 1 || j == 0 || j == n - 1) &&
      !(i == startI && j == startJ) &&
      maze(i)(j) == '.'
    }

    def next(i: Int, j: Int): List[(Int, Int)] =
      directions.map { case (di, dj) =>
        (i + di, j + dj)
      }

    queue += ((startI, startJ, 0, List((startI, startJ))))
    while (queue.nonEmpty) {
      val (i, j, steps, path) = queue.dequeue()
      println((i, j, steps))
      printArr(i, j, path, maze)

      if (isExit(i, j)) return steps

      next(i, j).foreach { case (nextI, nextJ) =>
        if (isValid(nextI, nextJ)) {
          queue += ((nextI, nextJ, steps + 1, (nextI, nextJ) :: path))
          visited(nextI)(nextJ) = true
        }
      }
    }

    -1
  }
}
