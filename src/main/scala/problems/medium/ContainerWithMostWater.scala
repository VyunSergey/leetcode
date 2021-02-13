package problems.medium

object ContainerWithMostWater {
  def main(args: Array[String]): Unit = {
    val height = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}
    Console.out.println(maxArea(height))
  }

  def maxArea(height: Array[Int]): Int = {
    val l = height.length
    var max = (l-1) * Math.min(height(0), height(l - 1))

    for {
      k <- Range(l - 1, 0, -1)
      i <- Range(0, l - k, 1) if k * Math.min(height(i), height(i + k)) > max
    } yield {
      max = Math.max(max, k * Math.min(height(i), height(i + k)))
    }

    max
  }
}
