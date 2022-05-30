package problems.medium

import scala.collection.mutable

object PacificAtlanticWaterFlow {
  private val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))

  def main(args: Array[String]): Unit = {
    val heights = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })

    val res = pacificAtlantic(heights)
    println(res)
  }

  def pacificAtlantic(heights: Array[Array[Int]]): List[List[Int]] = {
    val m = heights.length
    val n = heights.head.length
    val pacific = Array.fill(m)(Array.fill(n)(false))
    val atlantic = Array.fill(m)(Array.fill(n)(false))
    val res = mutable.ListBuffer.empty[List[Int]]

    def valid(i: Int, j: Int): Boolean = {
      0 <= i && i < m && 0 <= j && j < n
    }

    def dfs(i: Int, j: Int, grid: Array[Array[Boolean]]): Unit = {
      grid(i)(j) = true
      directions.foreach { case (di, dj) =>
        val (nextI, nextJ) = (i + di, j + dj)
        if (valid(nextI, nextJ) && heights(nextI)(nextJ) >= heights(i)(j) && !grid(nextI)(nextJ)) {
          dfs(nextI, nextJ, grid)
        }
      }
    }

    // Pacific
    (0 until m).foreach { i =>
      (0 until n).foreach { j =>
        if (i == 0 || j == 0) {
          dfs(i, j, pacific)
        }
      }
    }

    // Atlantic
    (m - 1 to 0 by -1).foreach { i =>
      (n - 1 to 0 by -1).foreach { j =>
        if (i == m - 1 || j == n - 1) {
          dfs(i, j, atlantic)
        }
      }
    }

    // both
    (0 until m).foreach { i =>
      (0 until n).foreach { j =>
        if (pacific(i)(j) && atlantic(i)(j)) res += List(i, j)
      }
    }

    res.result
  }
}
