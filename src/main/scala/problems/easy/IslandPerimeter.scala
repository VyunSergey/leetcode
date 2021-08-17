package problems.easy

import scala.util.Try

object IslandPerimeter {
  def main(args: Array[String]): Unit = {
    val grid: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    assert(grid.length > 0 && grid.length < 1000)
    assert(grid.head.length > 0 && grid.head.length < 1000)
    assert(grid.forall(_.length == grid.head.length))
    val res = islandPerimeter(grid)
    println(res)
  }

  def islandPerimeter(grid: Array[Array[Int]]): Int = {
    val N = grid.length
    val M = grid.head.length
    var perimeter = 0

    def edges(i: Int, j: Int): Int = {
      List((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
        .filter { case (k, l) => Try(grid(k)(l)).toOption.getOrElse(0) == 0 }
        .map(_ => 1)
        .sum
    }

    (0 until N).foreach { i =>
      (0 until M).foreach { j =>
        if (grid(i)(j) == 1) {
          perimeter += edges(i, j)
        }
      }
    }

    perimeter
  }

}
