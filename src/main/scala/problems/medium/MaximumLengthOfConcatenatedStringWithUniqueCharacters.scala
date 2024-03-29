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

  // "string" -> ["string".length to binary string]["string" to fixed 26 bits binary string]
  // "a"      ->     [1][00000000000000000000000001]
  // "ab"     ->    [10][00000000000000000000000011]
  // "abc"    ->    [11][00000000000000000000000111]
  // "a...z"  -> [11010][11111111111111111111111111]
  def wordToBitSet(optSet: mutable.HashSet[Int], word: String): Unit = {
    var charBitSet = 0

    word.toCharArray.foreach { chr =>
      // 'a' -> [00000000000000000000000001]
      // 'b' -> [00000000000000000000000010]
      // 'z' -> [10000000000000000000000000]
      val mask = 1 << chr - 'a'
      // check if word contains only unique characters
      if ((charBitSet & mask) > 0) return
      // add character to result bit set
      charBitSet += mask
    }

    // merge length and word
    optSet += charBitSet + (word.length << 26)
  }

  def dfs(optArr: Array[Int], pos: Int, res: Int): Int = {
    // "a...z"  -> [11010][11111111111111111111111111]
    // chars = [11111111111111111111111111]
    // len   = [11010]
    val oldChars = res & ((1 << 26) - 1)
    val oldLen = res >> 26
    var best = oldLen

    (pos until optArr.length).foreach { i =>
      val newChars = optArr(i) & ((1 << 26) - 1)
      val newLen = optArr(i) >> 26

      // check if words
      if ((newChars & oldChars) == 0) {
        // merge words and their lengths
        val newRes = newChars + oldChars + (newLen + oldLen << 26)
        // find next for new word
        best = Math.max(best, dfs(optArr, i + 1, newRes))
      }
    }
    best
  }
}
