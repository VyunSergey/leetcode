package problems.medium

import scala.collection.mutable

object SpiralMatrix {
  def main(args: Array[String]): Unit = {
    val matrix = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    val res = spiralOrder(matrix)
    println(res)
  }

  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    var n = matrix.length
    var m = matrix.headOption.map(_.length).getOrElse(0)
    var k = 0
    var l = 0
    var direction = 0
    val buf = mutable.ListBuffer.empty[Int]
    val moves = if (n <= m) 2 * n - 1 else 2 * m

    (0 until moves).foreach { _ =>
      // println(s"dir=$direction k=$k l=$l n=$n m=$m")
      // direction: 0 - right, 1 - down, 2 - left, 3 - up
      direction match {
        // right
        case 0 =>
          (0 until m).foreach { _ =>
            // println(s"m[$k][$l]=${matrix(k)(l)}")
            buf += matrix(k)(l)
            l += 1
          }
          k += 1
          l -= 1
          n -= 1
        // down
        case 1 =>
          (0 until n).foreach { _ =>
            // println(s"m[$k][$l]=${matrix(k)(l)}")
            buf += matrix(k)(l)
            k += 1
          }
          k -= 1
          l -= 1
          m -= 1
        // left
        case 2 =>
          (0 until m).foreach { _ =>
            // println(s"m[$k][$l]=${matrix(k)(l)}")
            buf += matrix(k)(l)
            l -= 1
          }
          k -= 1
          l += 1
          n -= 1
        // up
        case 3 =>
          (0 until n).foreach { _ =>
            // println(s"m[$k][$l]=${matrix(k)(l)}")
            buf += matrix(k)(l)
            k -= 1
          }
          k += 1
          l += 1
          m -= 1
        // error
        case d => throw new IllegalArgumentException(s"Wrong direction $d")
      }
      direction = (direction + 1) % 4
    }
    buf.result
  }
}
