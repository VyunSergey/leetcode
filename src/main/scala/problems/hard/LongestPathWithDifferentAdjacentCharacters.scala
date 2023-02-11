package problems.hard

import scala.collection.mutable

object LongestPathWithDifferentAdjacentCharacters {

  def main(args: Array[String]): Unit = {
    val parent: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }

    val s: String = Console.in.readLine()

    val res: Int = longestPath(parent, s)
    println(s"longest path is $res")
  }

  def longestPath(parent: Array[Int], s: String): Int = {
    val n = parent.length
    val children = Array.fill(n)(0)

    // start from node 1, since the root node does not have a parent.
    for(i <- 1 until n) children(parent(i)) += 1

    val queue = mutable.Queue.empty[Int]
    val chains = Array.fill(n, 2)(0)
    var path = 1

    for(i <- 1 until n) {
      // push all the leaf nodes in the queue
      if (children(i) == 0) {
        chains(i)(0) = 1
        queue += i
      }
    }

    while(queue.nonEmpty) {
      val i = queue.dequeue
      val p = parent(i)
      // get the number of nodes in the longest chain starting from the current node
      // including the current node
      val a = chains(i)(0)

      if (s(i) != s(p)) {
        // modify the longest chain and second longest chain if value is bigger
        if (a > chains(p)(0)) {
          chains(p)(1) = chains(p)(0)
          chains(p)(0) = a
        } else if (a > chains(p)(1)) {
          chains(p)(1) = a
        }
      }

      path = Math.max(path, chains(p)(0) + chains(p)(1) + 1)
      children(p) -= 1

      if (children(p) == 0 && p != 0) {
        chains(p)(0) += 1
        queue += p
      }
    }

    path
  }

}
