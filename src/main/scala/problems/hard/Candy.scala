package problems.hard

import scala.collection.mutable

object Candy {
  def main(args: Array[String]): Unit = {
    val ratings: Array[Int] = Console.in.readLine().split(",")
      .map(_.toIntOption).collect { case Some(x) => x }
    val res = candy(ratings)
    println(res)
  }

  def candy(ratings: Array[Int]): Int = {
    var count = 0
    val len = ratings.length
    val rtToInd = mutable.TreeMap.empty[Int, mutable.HashSet[Int]]
    val indToCandy = mutable.TreeMap.empty[Int, Int]

    (0 until len).foreach { i =>
      val rt = ratings(i)
      if (!rtToInd.contains(rt)) {
        rtToInd += (rt -> mutable.HashSet(i))
      } else {
        rtToInd(rt) += i
      }
      indToCandy += (i -> 1)
      count += 1
    }
    // println(("rt", rtToInd.view.mapValues(_.mkString("[", ",", "]")).toMap))

    rtToInd.keySet.foreach { rt =>
      // println(s"\nrt=$rt ids=${rtToInd(rt).mkString("[", ",", "]")}")
      rtToInd(rt).foreach { i =>
        var candies = indToCandy(i)
        // println(f"  i=$i%2d rt=$rt candies=$candies")
        if (i > 0) {
          val leftRt = ratings(i - 1)
          val leftCandies = indToCandy(i - 1)
          // println(f"i-1=${i - 1}%2d leftRt=$leftRt leftCandies=$leftCandies")
          if (leftRt > rt && leftCandies <= candies) {
            indToCandy += ((i - 1) -> (candies + 1))
            count += (candies - leftCandies + 1)
          } else if (leftRt < rt && leftCandies >= candies) {
            indToCandy += (i -> (leftCandies + 1))
            candies += (leftCandies - candies + 1)
            count += (leftCandies - candies + 1)
          }
        }
        if (i < len - 1) {
          val rightRt = ratings(i + 1)
          val rightCandies = indToCandy(i + 1)
          // println(f"i+1=${i + 1}%2d rightRt=$rightRt rightCandies=$rightCandies")
          if (rightRt > rt && rightCandies <= candies) {
            indToCandy += ((i + 1) -> (candies + 1))
            count += (candies - rightCandies + 1)
          } else if (rightRt < rt && rightCandies >= candies) {
            indToCandy += (i -> (rightCandies + 1))
            candies += (rightCandies - candies + 1)
            count += (rightCandies - candies + 1)
          }
        }
        // println(("candies", indToCandy))
      }
    }

    count
  }

}
