package problems.medium

import scala.collection.mutable

object ShortestBridge {
  private val directions: List[(Int, Int)] =
    List((-1, 0), (1, 0), (0, -1), (0, 1))

  def main(args: Array[String]): Unit = {
    val grid: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) if List(0, 1).contains(x) => x })
    printArr(grid)

    val res = shortestBridge(grid)
    println(s"res=$res")
  }

  def printArr(arr: Array[Array[Int]]): Unit = {
    println(arr.map(_.mkString).mkString("\n"))
    println()
  }

  def shortestBridge(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid.head.length
    val islands = Array.fill(m)(Array.fill(n)(0))
    val queue = mutable.Queue.empty[(Int, Int, Int)]
    val visited = Array.fill(m)(Array.fill(n)(false))
    var islandNum = 1

    def isValid(i: Int, j: Int): Boolean = {
      0 <= i && i < m && 0 <= j && j < n
    }

    def next(i: Int, j: Int): List[(Int, Int)] = {
      directions.map { case (di, dj) =>
        (i + di, j + dj)
      }.filter { case (x, y) =>
        isValid(x, y)
      }
    }

    // mark all island cells with `islandNum`
    def dfs(i: Int, j: Int, islandNum: Int): Unit = {
      islands(i)(j) = islandNum
      next(i, j).foreach { case (nextI, nextJ) =>
        if (islands(nextI)(nextJ) == 0 && grid(nextI)(nextJ) == 1) {
          dfs(nextI, nextJ, islandNum)
        }
      }
    }

    // find First and Second islands
    (0 until m).foreach { i =>
      (0 until n).foreach { j =>
        if (islands(i)(j) == 0 && grid(i)(j) == 1) {
          dfs(i, j, islandNum)
          islandNum += 1
        }
      }
    }

    printArr(islands)

    // enqueue all cell from First island
    (0 until m).foreach { i =>
      (0 until n).foreach { j =>
        if (islands(i)(j) == 1) {
          queue += ((i, j, 0))
        }
      }
    }

    while (queue.nonEmpty) {
      val (i, j, distance) = queue.dequeue()
      println((i, j, distance))

      next(i, j).foreach { case (nextI, nextJ) =>
        // if cell is Second island return the distance
        if (islands(nextI)(nextJ) == 2) return distance

        // moving forward to `0` or `2` unexplored cells
        if (islands(nextI)(nextJ) != 1 && !visited(nextI)(nextJ)) {
          queue += ((nextI, nextJ, distance + 1))
          visited(nextI)(nextJ) = true
        }
      }
    }

    -1
  }
}
