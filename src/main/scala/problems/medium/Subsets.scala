package problems.medium

import scala.annotation.tailrec
import scala.collection.mutable

object Subsets {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => x.toIntOption).collect { case Some(x) => x }
    println(
      subsetsWithDup(nums).sortBy(_.length)
        .map(_.mkString("[", ",", "]")).mkString("\n")
    )
  }

  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
    val sorted = nums.sorted.toList
    subsets(sorted, mutable.HashSet(List.empty[Int])).toList
  }

  @tailrec
  def subsets(lst: List[Int], res: mutable.HashSet[List[Int]]): mutable.HashSet[List[Int]] = {
    lst match {
      case h :: tail =>
        subsets(tail, res.flatMap(ls => List(ls, h :: ls)))
      case Nil => res
    }
  }
}
