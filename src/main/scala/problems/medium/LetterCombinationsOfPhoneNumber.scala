package problems.medium

object LetterCombinationsOfPhoneNumber {
  def main(args: Array[String]): Unit = {
    val digits = Console.in.readLine.filter(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').contains)
    Console.out.println(letterCombinations(digits).mkString("[", ",", "]"))
  }

  def letterCombinations(digits: String): List[String] = {
    val mm = Map(
      '1' -> List.empty[String],
      '2' -> List("a","b","c"),
      '3' -> List("d","e","f"),
      '4' -> List("g","h","i"),
      '5' -> List("j","k","l"),
      '6' -> List("m","n","o"),
      '7' -> List("p","q","r","s"),
      '8' -> List("t","u","v"),
      '9' -> List("w","x","y","z")
    )

    digits.toCharArray.foldLeft(List("")) { case (lst, d) =>
      mm.getOrElse(d, List.empty[String]).flatMap(s => lst.map(_ + s))
    }.filter(_.nonEmpty)
  }
}
