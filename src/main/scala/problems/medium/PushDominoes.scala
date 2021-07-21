package problems.medium

import scala.collection.mutable

object PushDominoes {
  def main(args: Array[String]): Unit = {
    val dominoes: String = Console.in.readLine().filter(List('L', 'R', '.').contains)
    val res: String = pushDominoes(dominoes)
    println(res)
  }

  def pushDominoes(dominoes: String): String = {
    var i = 0
    var finish = false
    val len = dominoes.length
    val updates = mutable.ArrayBuffer.empty[(Int, Char)]
    val chars: Array[Char] = dominoes.toCharArray

    while(!finish) {
      // println(s"upd=${updates.mkString("[", ",", "]")}\n")
      updates.foreach { case (i, c) => chars(i) = c }
      updates.clear()
      i = 0
      while(i < len) {
        // println(s"fin=$finish i=$i str=${chars.zipWithIndex.map{case (c, j) => if (j == i) s"[$c]" else s"$c"}.mkString}")
        // 'L' on the right to '.'
        if (i < len - 1 && chars(i) == '.' && chars(i + 1) == 'L') {
          // println(s" prev=${if (i > 0) chars(i - 1) else '_'} curr=${chars(i)} next=${chars(i + 1)}")
          // NO 'R' on the left to '.'
          if (i == 0 || chars(i - 1) != 'R') {
            // println(s"  save upd curr=${chars(i)} to 'L'")
            updates += ((i, 'L'))
          }
        }
        // 'R' on the left to '.'
        if (i > 0 && chars(i - 1) == 'R' && chars(i) == '.') {
          // println(s" prev=${chars(i - 1)} curr=${chars(i)} next=${if (i < len - 1) chars(i + 1) else '_'}")
          // NO 'L' on the right to '.'
          if (i == len - 1 || chars(i + 1) != 'L') {
            // println(s"  save upd curr=${chars(i)} to 'R'")
            updates += ((i, 'R'))
          }
        }
        i += 1
      }
      if (updates.isEmpty) finish = true
    }
    chars.mkString
  }
}
