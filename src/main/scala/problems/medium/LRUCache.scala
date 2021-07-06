package problems.medium

import scala.collection.mutable

object LRUCache {
  def main(args: Array[String]): Unit = {
    val commands: Array[String] = Console.in.readLine().split(" ")
    //val res: String = decodeString(string)
    println(commands.toList)
  }

  class LRUCache(_capacity: Int) {
    private var capacity = _capacity
    private val store = mutable.HashMap.empty[Int, (Int, Long)]

    def get(key: Int): Int = {
      store.get(key) match {
        case Some(v) =>
          store.update(key, (v._1, System.nanoTime()))
          v._1
        case None => -1
      }
    }

    def put(key: Int, value: Int) {
      store.get(key) match {
        case Some(_) =>
          store.update(key, (value, System.nanoTime()))
        case None =>
          if (capacity > 0) {
            store.update(key, (value, System.nanoTime()))
            capacity -= 1
          } else {
            val leastKey = store.view.mapValues(_._2).toList.minBy(_._2)._1
            store.remove(leastKey)
            store.update(key, (value, System.nanoTime()))
          }
      }
    }
  }

}
