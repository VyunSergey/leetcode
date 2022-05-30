package problems.hard

object EditDistance {
  def main(args: Array[String]): Unit = {
    val Array(word1, word2) = Console.in.readLine().split(" ").take(2)

    val res = minDistance(word1, word2)
    println(res)
  }

  def minDistance(word1: String, word2: String): Int = {
    val m = word1.length
    val n = word2.length

    // word1=abc word2=cde
    // a<->c, a->cd, a<->cde
    // ab<->c, ab<->cd, ab<->cde
    // abc<->c, abc<->cd, abc<->cde
    // ab[c]<->cd[e]: [ab]e<->[cd]e, [ab]c<->[cd]c, [ab]<->[cde], [abc]<->[cd]

    // dp(i)(j) = number of operations to equalise word1[0...i-1] and word2[0...j-1]
    //                           first i char`s |a.....z|  first j char`s |b.....g|
    val dp = Array.fill(m + 1)(Array.fill(n + 1)(0))

    // dp(i)(0) - number of operations to equalise word1[0...i-1] and ""
    (1 to m).foreach(i => dp(i)(0) = i)
    // dp(0)(j) - number of operations to equalise "" and word2[0...j-1]
    (1 to n).foreach(j => dp(0)(j) = j)

    // dp(i)(j) word1|...[i]|    word2|...[j]|
    // word1 - delete [i] - dp(i-1)(j) + 1
    // word2 - delete [j] - dp(i)(j-1) + 1
    // word1 - replace word1[i] with word2[j] - dp(i-1)(j-1) + 1
    // word2 - replace word2[j] with word1[i] - dp(i-1)(j-1) + 1
    (1 to m).foreach { i =>
      (1 to n).foreach { j =>

        if (word1(i - 1) == word2(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = List(
            dp(i - 1)(j - 1),
            dp(i)(j - 1),
            dp(i - 1)(j)
          ).min + 1
        }
        print(s"[$i][$j]=${dp(i)(j)}")
        print(" ")
      }
      println("\n")
    }

    dp(m)(n)
  }

}
