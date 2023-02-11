package problems.medium

import scala.collection.mutable

object NumberOfWeakCharactersInTheGame {

  def main(args: Array[String]): Unit = {
    val properties: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(i) => i }.take(2))

    val res = numberOfWeakCharacters(properties)
    println(s"The number of weak characters is $res")
  }

  def numberOfWeakCharacters(properties: Array[Array[Int]]): Int = {
    val sortX = properties.sortBy{case Array(x, y) => (-x, -y)}
    val sortY = properties.sortBy{case Array(x, y) => (-y, -x)}

    val mapX = mutable.HashMap.empty[Int, Int]
    val mapY = mutable.HashMap.empty[Int, Int]

    var maxX = 0
    var maxY = 0

    var ans = 0

    for(p <- sortX) {
      if (p(1) > maxY) maxY = p(1)
      mapX += p(0) -> maxY
    }

    for(p <- sortY) {
      if (p(0) > maxX) maxX = p(0)
      mapY += p(1) -> maxX
    }

    for(p <- properties if p(1) < mapX(p(0)) && p(0) < mapY(p(1))) ans += 1

    ans
  }

}
