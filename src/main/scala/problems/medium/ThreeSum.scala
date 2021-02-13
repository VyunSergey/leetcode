package problems.medium

object ThreeSum {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}
    Console.out.println(threeSum(nums).map(_.mkString("[", ",", "]")).sorted.mkString("[", ",", "]"))
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val numsG = nums.groupMapReduce(identity)(_ => 1)(_+_)
    val st = scala.collection.mutable.Set.empty[List[Int]]

    for {
      (a, i) <- numsG
      (b, j) <- numsG if (a < b) && numsG.contains(-(a + b)) && (
        (a == -(a + b) && i > 1) ||
          (b == -(a + b) && j > 1) ||
          (a != -(a + b) && b != -(a + b))
        )
    } yield {
      val c = -(a + b)
      val l = if (b < c) List(a, b, c) else if (a < c) List(a, c, b) else List(c, a, b)
      st.add(l)
    }

    if (numsG.getOrElse(0,0) > 2) st.toList :+ List(0,0,0) else st.toList
  }
}
