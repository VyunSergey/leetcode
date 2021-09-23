package problems.medium

import scala.collection.mutable

object MaximumLengthOfConcatenatedStringWithUniqueCharacters {
  def main(args: Array[String]): Unit = {
    val arr: List[String] = Console.in.readLine().split(" ").toList
    val res = maxLength(arr)
    println(res)
  }

  def maxLength(arr: List[String]): Int = {
    val optSet = mutable.HashSet.empty[Int]
    arr.foreach(word => wordToBitSet(optSet, word))
    dfs(optSet.toArray, 0, 0)
  }

  def wordToBitSet(optSet: mutable.HashSet[Int], word: String): Unit = {
    var charBitSet = 0

    word.toCharArray.foreach { chr =>
      val mask = 1 << chr - 'a'
      if ((charBitSet & mask) > 0) return
      charBitSet += mask
    }

    optSet += charBitSet + (word.length << 26)
  }

  def dfs(optArr: Array[Int], pos: Int, res: Int): Int = {
    val oldChars = res & ((1 << 26) - 1)
    val oldLen = res >> 26
    var best = oldLen

    (pos until optArr.length).foreach { i =>
      val newChars = optArr(i) & ((1 << 26) - 1)
      val newLen = optArr(i) >> 26

      if ((newChars & oldChars) == 0) {
        val newRes = newChars + oldChars + (newLen + oldLen << 26)
        best = Math.max(best, dfs(optArr, i + 1, newRes))
      }
    }
    best
  }
}
