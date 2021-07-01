package problems.medium

object TaskScheduler {
  def main(args: Array[String]): Unit = {
    val tasks: Array[Char] = Console.in.readLine().toCharArray.filter(_.isLetter)
    val n: Int = Console.in.readLine().toInt
    val res = leastInterval(tasks, n)
    println(res)
  }

  def leastInterval(tasks: Array[Char], n: Int): Int = {
    val chars = tasks.groupMapReduce(identity)(_ => 1)(_ + _).values.toList.sortBy(x => -x)
    val max = chars.takeWhile(_ == chars.head).size
    math.max(tasks.length, (chars.head - 1) * (n + 1) + max)
  }
}
