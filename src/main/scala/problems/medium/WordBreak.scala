package problems.medium

import scala.collection.mutable

object WordBreak {
  def main(args: Array[String]): Unit = {
    val string: String = Console.in.readLine()
    val wordDict: List[String] = Console.in.readLine().split(" ").toList
    val res: Boolean = wordBreak(string, wordDict)
    println(res)
  }

  // creating a list of cartesian product elements taken from each input list
  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case head :: tail =>
      for {
        // one element from head list
        xh <- head
        // rest of elements from tail cartesian product
        xt <- cartesianProduct(tail)
      } yield xh :: xt
  }

  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    implicit val stringOrdering: Ordering[List[String]] =
      (x: List[String], y: List[String]) => y.map(_.length).sum - x.map(_.length).sum
    var list = List.empty[String]
    var lists = List.empty[List[String]]
    var splits = List.empty[List[List[String]]]
    val queue = mutable.PriorityQueue.empty[List[String]]

    // adding initial string to PriorityQueue
    queue.enqueue(List(s))
    // loop while PriorityQueue contains something
    while(queue.nonEmpty) {
      // getting list of string parts from PriorityQueue
      list = queue.dequeue()
      // println(s"list=$list, queue=$queue, size=${queue.size}")

      // checking if all parts of string will be in wordDict
      // if we will split them by some word in wordDict
      if (list.forall { str =>
        wordDict.exists { word =>
          str.split(word).filter(_.nonEmpty).forall(wordDict.contains)
        }
      }) return true
      // split all parts of string only if
      // all of them contains some word in wordDict
      if (list.forall(str => wordDict.exists(str.contains))) {
        // each part of string maps into a list of splits by words in wordDict
        splits = list.map { str =>
          wordDict.filter(str.contains)
            .map(word => str.split(word).filter(_.nonEmpty).toList)
            .filter(_.forall(str => wordDict.exists(str.contains)))
        }.filter(_.exists(_.nonEmpty))
        // println(s"splits:\n${splits.map(_.mkString("[", ", ", "]")).mkString("\n")}")

        if (splits.nonEmpty) {
          // creating a list of new parts of string by chosen split above
          lists = cartesianProduct(splits).map(_.reduce(_ ++ _))
          // println(s"lists:\n${lists.mkString("\n")}")

          // checking if some parts of string are all in wordDict
          if (lists.exists(_.forall(wordDict.contains))) return true
          queue.enqueue(lists: _*)
        }
      }
    }
    false
  }
}
