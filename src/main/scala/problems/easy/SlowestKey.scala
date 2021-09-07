package problems.easy

object SlowestKey {

  def main(args: Array[String]): Unit = {
    val keysPressed = Console.in.readLine()
    val releaseTimes = Console.in.readLine().split(",")
      .map(_.toIntOption).collect { case Some(x) => x }
    assert(keysPressed.length == releaseTimes.length)
    val res = slowestKey(releaseTimes, keysPressed)
    println(res)
  }

  def slowestKey(releaseTimes: Array[Int], keysPressed: String): Char = {
    val chars = keysPressed.toCharArray.toVector
    val times = Vector.range(0, releaseTimes.length).map { i =>
      releaseTimes(i) - (if (i > 0) releaseTimes(i - 1) else 0)
    }
    times.zip(chars).max._2
  }
}
