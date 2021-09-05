
package object common {
  def cut(s: String)(n: Int): String = {
    if (s.length < n) s else s.slice(0, n - 3) + "..."
  }

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

  def profile[A, B](fun1: A => B, fun2: A => B)(data: List[A])
                   (implicit eq: Equiv[B]): List[(Boolean, Long, Long)] = {
    data.map { a =>
      val (b1, t1) = time(fun1(a))
      val (b2, t2) = time(fun2(a))
      (eq.equiv(b1, b2), t1, t2)
    }
  }

  def profileStats[A, B](fun1: A => B, fun2: A => B)(data: List[A])
                  (implicit eq: Equiv[B]): (Boolean, String, String, String, String, String, String) = {
    val prof = profile(fun1, fun2)(data)
    val len = prof.length
    val (bool, min1, sum1, max1, min2, sum2, max2) =
      prof.foldLeft((true, 0L, 0L, 0L, 0L, 0L, 0L)) { case ((bool, min1, sum1, max1, min2, sum2, max2), (bl, t1, t2)) =>
        (bool && bl,
          Math.min(min1, t1), sum1 + t1, Math.max(max1, t1),
          Math.min(min2, t2), sum2 + t2, Math.max(max2, t2)
        )
      }
    (if (len > 0) bool else false,
      nanoToTimestamp(min1),
      nanoToTimestamp(if (len > 0) (sum1.toDouble / len.toDouble).toLong else sum1),
      nanoToTimestamp(max1),
      nanoToTimestamp(min2),
      nanoToTimestamp(if (len > 0) (sum2.toDouble / len.toDouble).toLong else sum2),
      nanoToTimestamp(max2)
    )
  }
}
