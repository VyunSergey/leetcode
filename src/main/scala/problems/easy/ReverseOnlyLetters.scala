package problems.easy

object ReverseOnlyLetters {
  def main(args: Array[String]): Unit = {
    val str = Console.in.readLine()
    val res = reverseOnlyLetters(str)
    println(res)
  }

  def reverseOnlyLetters(s: String): String = {
    s.zipWithIndex
      .foldLeft(List((List.empty[(Char, Int)], List.empty[(Char, Int)]))) {
        case (List((letters, other)), (chr, i)) =>
          if (chr.isLetter) List(((chr, i) :: letters, other))
          else List((letters, (chr, i) :: other))
      }.map { case (letters, other) =>
      val ordered = letters.zip(letters.reverse).map { case ((chr, _), (_, i)) => (chr, i) }
      (ordered ++ other).sortBy(_._2).map(_._1).mkString
    }.foldLeft("") { case (str, lst) =>
      str + lst
    }
  }
}
