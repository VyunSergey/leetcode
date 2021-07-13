package problems.medium

import scala.collection.mutable

object IsomorphicStrings {
  def main(args: Array[String]): Unit = {
    val Array(str1, str2) = Console.in.readLine().split(" ").take(2)
    val res: Boolean = isIsomorphic(str1, str2)
    println(res)
  }

  def isIsomorphic(s: String, t: String): Boolean = {
    val mapStoT = mutable.Map.empty[Int, Int]
    val mapTtoS = mutable.Map.empty[Int, Int]

    var i = 0
    while(i < s.length) {
      val (c1, c2) = (s.charAt(i), t.charAt(i))

      // Case 1: No mapping exists in either of the dictionaries
      if (!mapStoT.contains(c1) && !mapTtoS.contains(c2)) {
        mapStoT += (c1.toInt -> c2.toInt)
        mapTtoS += (c2.toInt -> c1.toInt)
      }
      // Case 2: Ether mapping doesn't exist in the dictionaries or Mapping exists and
      // it doesn't match in either of the dictionaries or both
      else if (!(mapStoT.getOrElse(c1, -1) == c2 && mapTtoS.getOrElse(c2, -1) == c1)) {
        return false
      }
      i += 1
    }
    true
  }
}
