package problems.easy

object TwoSum {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}
    val target = Console.in.readLine.toInt
    Console.out.println(twoSum(nums, target).mkString("[", ",", "]"))
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    (for {
      (a,i) <- nums.zipWithIndex
      (b,j) <- nums.zipWithIndex if (j > i) && (a + b == target)
    } yield Array(i, j)).flatten
  }
}
