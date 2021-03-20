package problems.easy

import scala.collection.mutable

object DistinctImplementation {
  def main(args: Array[String]): Unit = {
    val seq: Seq[Int] = Console.in.readLine.split("\\s")
      .collect { case str if str.toIntOption.isDefined => str.toInt }.toSeq

    val distinctSeq = distinct(seq)
    val distinct2Seq = distinct2(seq)

    println(distinctSeq)
    println(distinct2Seq)
  }

  def distinct(seq: Seq[Int]): Seq[Int] = {
    // (1, 2, 1, 3, 4, 5)
    // ((1, 0), (2, 1), (1, 2), (3, 3), (4, 4), (5, 5))
    // (1 -> ((1, 0), (1, 2)), 2 -> ((2, 1)), 3 -> ((3, 3)), 4 -> ((4, 4)), 5 -> ((5, 5))
    // (1 -> (1, 0), 2 -> (2, 1), 3 -> (3, 3), 4 -> (4, 4), 5 -> (5, 5))
    // ((1, 0), (2, 1), (3, 3), (4, 4), (5, 5))
    // (1, 2, 3, 4, 5)
    seq.zipWithIndex
      .groupBy(_._1)
      .view.mapValues(_.minBy(_._2))
      .values.toSeq
      .sortBy(_._2)
      .map(_._1)
  }

  def distinct2(seq: Seq[Int]): Seq[Int] = {
    val set = mutable.HashSet.empty[Int]
    seq.collect { case a if !set.contains(a) => set.add(a); a }
  }

}
