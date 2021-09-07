package problems.hard

import scala.collection.mutable

object SumDistancesInTree {

  case class TreeNode(value: Int, children: Array[TreeNode] = Array.empty[TreeNode])

  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val edges: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x }.take(2))
    assert(edges.length == n - 1)
    assert(edges.forall(_.length == 2))
    assert(edges.flatten.toSet == Set.range(0, n))
    val res = sumOfDistancesInTree(n, edges)
    println(res.mkString("[", ",", "]"))
  }

  def sumOfDistancesInTree(n: Int, edges: Array[Array[Int]]): Array[Int] = {
    val graph = Vector.fill(n)(mutable.HashSet.empty[Int])
    val ans = Array.fill(n)(0)
    val count = Array.fill(n)(1)

    def dfs(node: Int, parent: Int): Unit = {
      graph(node).foreach { child =>
        if (child != parent) {
          dfs(child, node)
          count(node) += count(child)
          ans(node) += ans(child) + count(child)
        }
      }
    }

    def dfs2(node: Int, parent: Int): Unit = {
      graph(node).foreach { child =>
        if (child != parent) {
          ans(child) = ans(node) - count(child) + n - count(child)
          dfs2(child, node)
        }
      }
    }

    edges.foreach { case Array(a, b) =>
      graph(a) += b
      graph(b) += a
    }

    dfs(0, -1)
    dfs2(0, -1)
    ans
  }

  def sumOfDistancesInTree2(n: Int, edges: Array[Array[Int]]): Array[Int] = {
    if (edges.isEmpty) return Array(0)
    else if (edges.length == 1) return Array(1, 1)

    val sortedEdges = edges.map { case Array(a, b) => (Math.min(a, b), Math.max(a, b)) }.sorted
    val valToId = mutable.TreeMap.empty[Int, Int]
    val root = findRoot(sortedEdges)
    // println(s"root=$root")

    valToId += (root -> 0)
    val tree = makeTree(root, valToId, sortedEdges)
    // println(valToId)
    // println(showTree(tree))

    val distances = mutable.TreeMap.empty[(Int, Int), Int]
    val paths = mutable.TreeMap.empty[Int, Set[Int]]
    distances += ((tree.value, tree.value) -> 0)
    findDistances(tree, Set.empty[Int], distances, paths)
    // distances.foreach(println)
    // paths.foreach(println)

    (0 until n).map { i =>
      val ind = valToId(i)
      (0 until n).map(j => distance(ind, j, distances, paths)).sum
    }.toArray
  }

  def findRoot(edges: Array[(Int, Int)]): Int = {
    edges.map(_._1).find(x => !edges.exists(_._2 == x)).getOrElse(0)
  }

  def makeTree(value: Int, valToId: mutable.TreeMap[Int, Int], edges: Array[(Int, Int)]): TreeNode = {
    var index = 0

    def mkTree(value: Int): TreeNode = {
      val childrenIds = edges.filter { case (a, b) =>
        (a == value && !valToId.contains(b)) ||
          (b == value && !valToId.contains(a))
      }.map { case (a, b) => a + b - value }
        .map { v =>
          index += 1
          (v, index)
        }

      // println(s"value=$value ind=${valToId(value)} index=$index child=${childrenIds.toList} valToId=$valToId")
      valToId ++= childrenIds
      TreeNode(valToId(value), childrenIds.map(kv => mkTree(kv._1)))
    }

    mkTree(value)
  }

  def showTree(tree: TreeNode): String = {
    if (tree.children.isEmpty) s"(${tree.value})"
    else s"(${tree.value},${tree.children.map(showTree).mkString("[", ",", "]")})"
  }

  def findDistances(tree: TreeNode, path: Set[Int], dist: mutable.TreeMap[(Int, Int), Int],
                    paths: mutable.TreeMap[Int, Set[Int]]): Unit = {
    dist ++= dist.filter { case ((_, b), _) =>
      b == tree.value
    }.flatMap { case ((a, _), count) =>
      tree.children.map { node =>
        ((a, node.value), count + 1)
      }.toMap
    }

    dist ++= tree.children.map { node =>
      ((tree.value, node.value), 1)
    }.toMap

    paths += tree.value -> (path ++ Set(tree.value))

    tree.children.foreach { node =>
      findDistances(node, path ++ Set(tree.value), dist, paths)
    }
  }

  def distance(i: Int, j: Int, dist: mutable.TreeMap[(Int, Int), Int],
               paths: mutable.TreeMap[Int, Set[Int]]): Int = {
    if (i == j) 0
    else {
      val parent = paths(i).intersect(paths(j)).max
      // println(s"i=$i j=$j parent=$parent")
      (if (parent == i) 0 else dist((parent, i))) +
        (if (parent == j) 0 else dist((parent, j)))
    }
  }
}
