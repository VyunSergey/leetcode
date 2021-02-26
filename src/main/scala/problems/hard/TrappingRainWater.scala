package problems.hard

object TrappingRainWater {
  def element[A](arr: Array[A], index: Int): Option[A] = {
    scala.util.Try(arr(index)).toOption
  }

  def getPoints(arr: Array[Int]): Array[(Int,Int)] = {
    arr.zipWithIndex.filter { case (x, i) =>
      val (prev, next) = (
        element(arr, i-1).getOrElse(0),
        element(arr, i+1).getOrElse(0)
      )
      prev <= x && x >= next
    }
  }

  def clearPoints(arr: Array[(Int,Int)]): Array[(Int,Int)] = {
    if (arr.length < 3) arr
    else {
      arr.zipWithIndex.filterNot { case ((x, _), j) =>
        val (prev, next) = (
          element(arr, j-1).map(_._1).getOrElse(0),
          element(arr, j+1).map(_._1).getOrElse(0)
        )
        prev >= x && x <= next
      }.map(_._1)
    }
  }

  def clearWhile(arr: Array[(Int,Int)]): Array[(Int,Int)] = {
    var (prev, cleared) = (arr, clearPoints(arr))
    var diff = cleared.diff(prev)
    while(diff.nonEmpty || prev.length != cleared.length) {
      prev = cleared
      cleared = clearPoints(cleared)
      diff = cleared.diff(prev)
    }
    cleared
  }

  def volume(arr: Array[Int], level: Int): Int = {
    arr.map(a => if (level > a) level - a else 0).sum
  }

  def sumPoints(arr: Array[Int], points: Array[(Int,Int)]): Int = {
    val len = points.length
    if (len == 1) 0
    else if (len == 2) {
      val Array((x,i), (y,j)) = points
      volume(arr.slice(i+1, j), Math.min(x, y))
    } else {
      points.sliding(2,1).map { case Array((x,i), (y,j)) =>
        volume(arr.slice(i+1, j), Math.min(x, y))
      }.sum
    }
  }

  def trap(height: Array[Int]): Int = {
    if (height.length > 2) {
      val points = getPoints(height)
      val pointsCleared = clearWhile(points)
      sumPoints(height, pointsCleared)
    }
    else 0
  }
}
