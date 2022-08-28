package problems.hard

import scala.collection.mutable

object ProcessRestrictedFriendRequests {
  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val restrictions: Array[Array[Int]] = Console.in.readLine().split("\\s+").map(_.split(",").map(_.toInt).take(2))
    val requests: Array[Array[Int]] = Console.in.readLine().split("\\s+").map(_.split(",").map(_.toInt).take(2))
    println(s"n=$n")
    println(s"restrictions=${restrictions.map(_.mkString("[", ",", "]")).mkString(",")}")
    println(s"requests=${requests.map(_.mkString("[", ",", "]")).mkString(",")}")

    val res: Array[Boolean] = friendRequests(n, restrictions, requests)
    println(s"res=${res.mkString("[", "][", "]")}")
  }

  def friendRequests(n: Int, restrictions: Array[Array[Int]], requests: Array[Array[Int]]): Array[Boolean] = {
    val m = requests.length
    val ans: Array[Boolean] = Array.fill(m)(false)
    val who: Array[Int] = Array.fill(n)(-1)
    val group: mutable.HashMap[Int, mutable.HashSet[Int]] =
      mutable.HashMap.empty[Int, mutable.HashSet[Int]]
    val bad: mutable.HashMap[Int, mutable.HashSet[Int]] =
      mutable.HashMap.empty[Int, mutable.HashSet[Int]]
    var num = 0

    for(u <- 0 until n) {
      bad += u -> mutable.HashSet.empty[Int]
      group += u -> mutable.HashSet.empty[Int]
    }

    for(Array(u, v) <- restrictions) {
      bad(u) += v
      bad(v) += u
    }

    bad.foreach { case (u, set) =>
      println(s"$u bad=$set")
    }

    for(i <- 0 until m) {
      val Array(u, v) = requests(i)
      println(s"\ni=$i u=$u v=$v who($u)=${who(u)} who($v)=${who(v)}")
      (who(u), who(v)) match {
        // no groups
        case (-1, -1) =>
          // println("(-1 -1)")
          if (!bad(u).contains(v) && !bad(v).contains(u)) {
            who(u) = num
            who(v) = num
            group(num) ++= Seq(u, v)
            num += 1
            ans(i) = true
          }
        // one group for u
        case (wu, -1) =>
          // println(s"($wu -1)")
          if (group(wu).forall(w => !bad(w).contains(v)) && !bad(v).contains(u)) {
            who(v) = wu
            group(wu) += v
            ans(i) = true
          }
        // one group for v
        case (-1, wv) =>
          // println(s"(-1 $wv)")
          if (!bad(u).contains(v) && group(wv).forall(w => !bad(w).contains(u))) {
            who(u) = wv
            group(wv) += u
            ans(i) = true
          }
        // one the same group for u & v
        case (x, y) if x == y =>
          // println(s"($x $y)")
          ans(i) = true
        // two groups for u & v
        case (x, y) =>
          // println(s"($x $y)")

          // group(x).foreach(w => println(s"$w bad=${bad(w)}"))
          // println()
          // group(y).foreach(w => println(s"$w bad=${bad(w)}"))

          val badX = group(x).map(w => bad(w)).reduce(_ ++ _)
          val badY = group(y).map(w => bad(w)).reduce(_ ++ _)

          // println(s"badX=$badX badY=$badY")

          if ((badX intersect group(y)).isEmpty && (badY intersect group(x)).isEmpty) {
            val (w, z) = (Math.min(x, y), Math.max(x, y))
            // println(s"$z -> $w ${if (x == z) group(x) else group(y)} -> ${if (x == w) group(x) else group(y)}")

            group(x).foreach(u => who(u) = w)
            group(y).foreach(u => who(u) = w)
            group(w) ++= (group(x) ++ group(y))
            group -= z
            ans(i) = true
          }
      }
      println(s"i=$i who=${who.mkString("[", "][", "]")} group=${group.filter(_._2.nonEmpty)}")
    }

    ans
  }
}
