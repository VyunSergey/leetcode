package problems.hard

import scala.annotation.tailrec

object Game24 {
  def main(args: Array[String]): Unit = {
    val cards: Array[Int] = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => x.toIntOption).collect { case Some(x) => x }
    assert(cards.length == 4)
    val res: Boolean = judgePoint24(cards)
    println(res)
  }

  val target = 24
  val equals: Double => Boolean = (x: Double) => Math.abs(x - target) < 0.0000001
  val symOperators: List[(Double, Double) => Double] = List(_ + _, _ * _)
  val asymOperators: List[(Double, Double) => Double] = List(_ - _, _ / _)
  val operators: List[(Double, Double) => Double] = symOperators ++ asymOperators

  // [a, b, c, d] [+, -, *, /]
  // one argument traversing
  // a [+, *] ([b, c, d] [+, -, *, /])
  // a [-, /] ([b, c, d] [+, -, *, /]) OR ([b, c, d] [+, -, *, /]) [-, /] a
  // two arguments traversing
  // ([a, b] [+, -, *, /]) [+, -, *, /] ([c, d] [+, -, *, /])
  def judgePoint24(cards: Array[Int]): Boolean = {
    cards.toList.map(_.toDouble).permutations.foreach {
      case a :: b :: c :: d :: Nil =>
        // println(s"cards=${lst.mkString("[", ", ", "]")}")
        if (combinationsOne(b :: c :: d :: Nil, a :: Nil).exists(equals)) return true
        operators.foreach { f =>
          if (combinationsTwo(c :: d :: Nil, f(a, b) :: Nil).exists(equals)) return true
        }
      case lst => throw new IllegalArgumentException(s"Wrong input $lst")
    }
    false
  }

  @tailrec
  def combinationsOne(cards: List[Double], res: List[Double]): List[Double] = {
    cards match {
      case h :: tail =>
        combinationsOne(tail,
          res.flatMap { x =>
            asymOperators.flatMap(f => List(f(x, h), f(h, x)) ++
              symOperators.map(f => f(x, h)))
          }
        )
      case Nil =>
        // println(s"nums(${res.length})=${res.mkString("[", ", ", "]")}")
        res
    }
  }

  @tailrec
  def combinationsTwo(cards: List[Double], res: List[Double]): List[Double] = {
    cards match {
      case a :: b :: tail =>
        combinationsTwo(tail,
          res.flatMap { x =>
            operators.flatMap(f => operators.map(g => f(x, g(a, b))))
          }
        )
      case a :: Nil =>
        throw new IllegalArgumentException(s"Wrong input $a from $cards")
      case Nil =>
        // println(s"nums(${res.length})=${res.mkString("[", ", ", "]")}")
        res
    }
  }
}
