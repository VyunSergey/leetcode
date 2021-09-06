package problems.hard

import common._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class SumDistancesInTreeTest extends AnyFlatSpec with Matchers {

  val test0: (Int, Array[Array[Int]]) = (2, Array(Array(1,0)))
  val test1: (Int, Array[Array[Int]]) = (3, Array(Array(2,1),Array(0,2)))
  val test2: (Int, Array[Array[Int]]) = (3, Array(Array(2,0),Array(1,0)))
  val test3: (Int, Array[Array[Int]]) = (5, Array(Array(2,0),Array(4,2),Array(3,1),Array(1,0)))
  val test4: (Int, Array[Array[Int]]) = (9,
    Array(
      Array(7,1),Array(7,3),Array(3,2),Array(3,6),
      Array(3,5),Array(2,4),Array(2,0),Array(8,1))
  )
  val test5: (Int, Array[Array[Int]]) = (200,
    Array(
      Array(112,91),Array(158,18),Array(183,96),Array(179,69),Array(183,157),
      Array(170,154),Array(173,162),Array(24,102),Array(140,100),Array(80,115),
      Array(87,154),Array(79,174),Array(46,115),Array(30,194),Array(129,9),
      Array(101,82),Array(170,26),Array(33,123),Array(172,124),Array(2,8),
      Array(148,149),Array(16,98),Array(174,48),Array(128,76),Array(175,87),
      Array(16,127),Array(171,124),Array(20,69),Array(128,159),Array(198,192),
      Array(99,63),Array(151,79),Array(194,155),Array(53,26),Array(2,133),
      Array(56,70),Array(154,191),Array(78,38),Array(137,70),Array(116,135),
      Array(69,176),Array(127,9),Array(14,1),Array(169,56),Array(187,125),
      Array(67,20),Array(59,33),Array(24,69),Array(89,126),Array(168,174),
      Array(110,194),Array(39,81),Array(58,191),Array(188,8),Array(51,166),
      Array(167,96),Array(23,96),Array(58,131),Array(122,20),Array(33,42),
      Array(25,0),Array(47,33),Array(37,132),Array(166,195),Array(57,131),
      Array(131,79),Array(78,92),Array(7,147),Array(62,68),Array(120,113),
      Array(172,80),Array(177,83),Array(154,197),Array(11,106),Array(79,147),
      Array(60,131),Array(161,85),Array(18,95),Array(152,80),Array(93,186),
      Array(145,175),Array(196,41),Array(39,27),Array(57,165),Array(150,29),
      Array(118,97),Array(51,56),Array(22,84),Array(161,32),Array(175,182),
      Array(171,45),Array(156,81),Array(113,162),Array(187,65),Array(32,153),
      Array(12,51),Array(163,54),Array(147,143),Array(142,9),Array(26,141),
      Array(163,33),Array(51,0),Array(172,91),Array(174,185),Array(131,113),
      Array(100,17),Array(51,50),Array(94,160),Array(44,111),Array(154,187),
      Array(170,157),Array(6,165),Array(72,52),Array(196,71),Array(134,100),
      Array(121,1),Array(61,146),Array(101,118),Array(126,166),Array(3,46),
      Array(87,80),Array(14,154),Array(178,39),Array(32,117),Array(40,61),
      Array(191,107),Array(98,119),Array(19,174),Array(67,74),Array(169,130),
      Array(105,39),Array(17,87),Array(56,64),Array(57,101),Array(135,150),
      Array(83,164),Array(34,197),Array(38,81),Array(87,128),Array(31,33),
      Array(21,69),Array(28,0),Array(12,88),Array(172,103),Array(51,114),
      Array(151,49),Array(73,4),Array(62,162),Array(56,139),Array(71,166),
      Array(72,3),Array(33,56),Array(136,129),Array(106,124),Array(84,13),
      Array(37,99),Array(56,66),Array(166,69),Array(109,90),Array(4,113),
      Array(46,36),Array(111,22),Array(127,160),Array(43,67),Array(80,198),
      Array(20,189),Array(37,127),Array(32,166),Array(87,29),Array(14,9),
      Array(86,131),Array(79,8),Array(5,16),Array(186,91),Array(144,191),
      Array(190,86),Array(179,180),Array(30,57),Array(177,51),Array(6,181),
      Array(10,170),Array(38,29),Array(55,138),Array(104,4),Array(55,172),
      Array(154,61),Array(95,136),Array(193,65),Array(22,90),Array(35,137),
      Array(199,161),Array(22,174),Array(46,184),Array(99,108),Array(75,65),
      Array(56,149),Array(58,51),Array(77,79),Array(15,106))
  )

  val expected0: Array[Int] = Array(1,1)
  val expected1: Array[Int] = Array(3,3,2)
  val expected2: Array[Int] = Array(2,3,3)
  val expected3: Array[Int] = Array(6,7,7,10,10)
  val expected4: Array[Int] = Array(23,21,16,13,23,20,20,16,28)
  val expected5: Array[Int] = Array(1204,1300,1568,1768,1402,1832,1592,1572,1372,1266,1326,
    1952,1206,1936,1104,1952,1634,1242,2038,1554,1520,1530,1542,1902,1528,1402,1322,1984,
    1402,1224,1394,1554,1346,1356,1342,1760,1772,1632,1406,1786,1340,1746,1554,1912,1936,
    1954,1574,1554,1554,1574,1208,1010,2162,1520,1750,1568,1172,1202,922,1554,1224,1142,
    1596,2024,1370,1332,1370,1714,1794,1332,1366,1352,1964,1600,1912,1530,1442,1378,1602,
    1180,1200,1594,1592,1400,1738,1738,1222,1050,1404,1552,1738,1564,1800,1958,1836,1842,
    1704,1788,1830,1826,1436,1394,1726,1570,1600,1984,1754,1130,2024,1936,1786,1738,1762,
    1208,1208,1386,1812,1544,1590,2028,1406,1498,1718,1554,1560,1336,1354,1442,1244,1456,
    1566,1026,1830,1766,1634,1614,1648,1562,1766,1370,1634,1520,1464,1572,1130,1442,1340,
    1374,1566,1368,1418,1376,1398,1544,948,1786,1792,1318,2236,1442,1638,1540,1400,1552,1598,
    1396,1158,1902,1554,1368,1128,1756,1372,1598,1356,1244,1530,1204,1984,1528,1726,1790,1442,
    1510,1772,1554,1760,1138,1570,1718,1420,932,1594,1530,1588,1356,1548,1144,1396,1738
  )

  def check(test: (Int, Array[Array[Int]]), expected: Array[Int], numTimes: Int = 1): Assertion = {
    println(s"Testing runs: $numTimes")
    val res = timedNTimes(numTimes)(SumDistancesInTree.sumOfDistancesInTree(test._1, test._2))
    res.sameElements(expected) shouldBe true
  }

  def checkParts(test: (Int, Array[Array[Int]]), expected: Array[Int], numTimes: Int = 1): Assertion = {
    println(s"Testing runs: $numTimes")
    val sortedEdges = test._2.map { case Array(a, b) => (Math.min(a, b), Math.max(a, b)) }.sorted
    val valToId = mutable.TreeMap.empty[Int, Int]
    println(s"Testing findRoot func:")
    val root = timedNTimes(numTimes)(SumDistancesInTree.findRoot(sortedEdges))
    println(s"Testing makeTree func:")
    val tree = timedNTimes(numTimes) {
      valToId.clear()
      valToId += (root -> 0)
      SumDistancesInTree.makeTree(root, valToId, sortedEdges)
    }
    val distances = mutable.TreeMap.empty[(Int, Int), Int]
    val paths = mutable.TreeMap.empty[Int, Set[Int]]
    println(s"Testing findDistances func:")
    timedNTimes(numTimes) {
      distances.clear()
      distances += ((tree.value, tree.value) -> 0)
      SumDistancesInTree.findDistances(tree, Set.empty[Int], distances, paths)
    }
    println(s"Testing sumDistances func:")
    val res = timedNTimes(numTimes) {
      (0 until test._1).map { i =>
        val ind = valToId(i)
        println(s"Testing sum $ind to all other")
        timed((0 until test._1).map(j => SumDistancesInTree.distance(ind, j, distances, paths)).sum)
      }.toArray
    }
    res.sameElements(expected) shouldBe true
  }

  "Test0" should "work in an acceptable amount of time" in {
    println(s"Test0: nodes=${test0._1}, edges=${test0._2.length}")
    check(test0, expected0, 100)
  }

  "Test1" should "work in an acceptable amount of time" in {
    println(s"Test1: nodes=${test1._1}, edges=${test1._2.length}")
    check(test1, expected1, 100)
  }

  "Test2" should "work in an acceptable amount of time" in {
    println(s"Test2: nodes=${test2._1}, edges=${test2._2.length}")
    check(test2, expected2, 100)
  }

  "Test3" should "work in an acceptable amount of time" in {
    println(s"Test3: nodes=${test3._1}, edges=${test3._2.length}")
    check(test3, expected3, 100)
  }

  "Test4" should "work in an acceptable amount of time" in {
    println(s"Test4: nodes=${test4._1}, edges=${test4._2.length}")
    check(test4, expected4, 100)
  }

  "Test5" should "work in an acceptable amount of time" in {
    println(s"Test5: nodes=${test5._1}, edges=${test5._2.length}")
    check(test5, expected5, 100)
  }

  "Test5-Parts" should "work in an acceptable amount of time" in {
    println(s"Test5: nodes=${test5._1}, edges=${test5._2.length}")
    checkParts(test5, expected5, 100)
  }
}
