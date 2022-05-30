package problems.common

object Timed {
  def time[A](block: => A): (A, Long) = {
    val t0: Long = System.nanoTime()
    val result: A = block
    val t1: Long = System.nanoTime()
    (result, t1 - t0)
  }

  def nanoToTimestamp(nano: Long): String = {
    val ms = nano / 1000000L
    val sec = ms / 1000L
    val min = sec / 60L
    val hr = min / 60L
    f"$hr%2dhr ${min - 60 * hr}%2dmn ${sec - 60 * min}%2dss ${ms - 1000 * sec}%3dms"
  }

  def timed[A](block: => A): A = {
    val (res, tm) = time(block)
    println(f"Elapsed time: ${nanoToTimestamp(tm)} | ${tm}ns")
    res
  }

  def timedNTimes[A](n: Int)(block: => A): A = {
    val (res, sumTm) = (0 until n).foldLeft(time(block)) { case ((_, tm), _) =>
      val (newRes, newTm) = time(block)
      (newRes, tm + newTm)
    }
    println(f"Elapsed time: ${nanoToTimestamp(sumTm / n)} | ${sumTm / n}ns")
    res
  }
}
