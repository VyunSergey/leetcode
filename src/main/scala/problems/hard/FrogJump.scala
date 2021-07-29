package problems.hard

import scala.collection.mutable

object FrogJump {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Console.in.readLine().split(" ").map(_.toIntOption).collect { case Some(n) => n }
    assert(arr.length > 0 && arr.head == 0)
    val res: Boolean = canCross(arr)
    println(res)
  }

  def canCross(stones: Array[Int]): Boolean = {
    var i = 0
    var stone = 0
    var reach = 0
    val len = stones.length
    val map = mutable.HashMap.empty[Int, mutable.HashSet[Int]]

    map += ((0, mutable.HashSet.empty[Int]))
    map(0) += 1
    i = 1
    while(i < len) {
      map += ((stones(i), mutable.HashSet.empty[Int]))
      i += 1
    }

    i = 0
    while(i < len) {
      // println(map)
      stone = stones(i)
      map(stone).foreach { step =>
        reach = step + stone
        if (reach == stones(len - 1)) {
          return true
        }
        map.get(reach).map { set =>
          set += step
          if (step - 1 > 0) set += step - 1
          set += step + 1
        }
      }
      i += 1
    }

    false
  }
}
