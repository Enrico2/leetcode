package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/cut-off-trees-for-golf-event/description/
  */
object CutOffTrees extends LeetcodeApp {

  import com.enricode.util.RanArrays.inBounds

  override def run(): Unit = {
    val forest1 = List(
      List(69438, 55243, 0, 43779, 5241, 93591, 73380),
      List(847, 49990, 53242, 21837, 89404, 63929, 48214),
      List(90332, 49751, 0, 3088, 16374, 70121, 25385),
      List(14694, 4338, 87873, 86281, 5204, 84169, 5024),
      List(31711, 47313, 1885, 28332, 11646, 42583, 31460),
      List(59845, 94855, 29286, 53221, 9803, 41305, 60749),
      List(95077, 50343, 27947, 92852, 0, 0, 19731),
      List(86158, 63553, 56822, 90251, 0, 23826, 17478),
      List(60387, 23279, 78048, 78835, 5310, 99720, 0),
      List(74799, 48845, 60658, 29773, 96129, 90443, 14391),
      List(65448, 63358, 78089, 93914, 7931, 68804, 72633),
      List(93431, 90868, 55280, 30860, 59354, 62083, 47669),
      List(81064, 93220, 22386, 22341, 95485, 20696, 13436),
      List(50083, 0, 89399, 43882, 0, 13593, 27847),
      List(0, 12256, 33652, 69301, 73395, 93440, 0),
      List(42818, 87197, 81249, 33936, 7027, 5744, 64710),
      List(35843, 0, 99746, 52442, 17494, 49407, 63016),
      List(86042, 44524, 0, 0, 26787, 97651, 28572),
      List(54183, 83466, 96754, 89861, 84143, 13413, 72921),
      List(89405, 52305, 39907, 27366, 14603, 0, 14104),
      List(70909, 61104, 70236, 30365, 0, 30944, 98378),
      List(20124, 87188, 6515, 98319, 78146, 99325, 88919),
      List(89669, 0, 64218, 85795, 2449, 48939, 12869),
      List(93539, 28909, 90973, 77642, 0, 72170, 98359),
      List(88628, 16422, 80512, 0, 38651, 50854, 55768),
      List(13639, 2889, 74835, 80416, 26051, 78859, 25721),
      List(90182, 23154, 16586, 0, 27459, 3272, 84893),
      List(2480, 33654, 87321, 93272, 93079, 0, 38394),
      List(34676, 72427, 95024, 12240, 72012, 0, 57763),
      List(97957, 56, 83817, 45472, 0, 24087, 90245),
      List(32056, 0, 92049, 21380, 4980, 38458, 3490),
      List(21509, 76628, 0, 90430, 10113, 76264, 45840),
      List(97192, 58807, 74165, 65921, 45726, 47265, 56084),
      List(16276, 27751, 37985, 47944, 54895, 80706, 2372),
      List(28438, 53073, 0, 67255, 38416, 63354, 69262),
      List(23926, 75497, 91347, 58436, 73946, 39565, 10841),
      List(34372, 69647, 44093, 62680, 32424, 69858, 68719),
      List(24425, 4014, 94871, 1031, 99852, 88692, 31503),
      List(24475, 12295, 33326, 37771, 37883, 74568, 25163),
      List(0, 18411, 88185, 60924, 29028, 69789, 0),
      List(34697, 75631, 7636, 16190, 60178, 39082, 7052),
      List(24876, 9570, 53630, 98605, 22331, 79320, 88317),
      List(27204, 89103, 15221, 91346, 35428, 94251, 62745),
      List(26636, 28759, 12998, 58412, 38113, 14678, 0),
      List(80871, 79706, 45325, 3861, 12504, 0, 4872),
      List(79662, 15626, 995, 80546, 64775, 0, 68820),
      List(25160, 82123, 81706, 21494, 92958, 33594, 5243)
    )
    val forest2 = List(List(2, 3, 4), List(0, 0, 5), List(8, 7, 6))

    val forest3 = List(
      List(54581641, 64080174, 24346381, 69107959),
      List(86374198, 61363882, 68783324, 79706116),
      List(668150, 92178815, 89819108, 94701471),
      List(83920491, 22724204, 46281641, 47531096),
      List(89078499, 18904913, 25462145, 60813308))

    val ans = cutOffTree(forest1)

    println(s"$ans")

  }

  import scala.collection.mutable

  def cutOffTree(_forest: List[List[Int]]): Int = {
    val forest = Array.ofDim[Int](_forest.length, _forest(0).length)
    for (i <- 0 until forest.length) {
      for (j <- 0 until forest(0).length) {
        forest(i)(j) = _forest(i)(j)
      }
    }

    case class SearchNode(i:Int, j:Int, dist: Int)
    case class Tree(i: Int, j: Int, height: Int)
    case class Node(i: Int, j: Int)
    val diffs = Seq((-1,0), (1, 0), (0, -1), (0, 1))

    val visited = Array.ofDim[Boolean](forest.length, forest(0).length)
    val q = new mutable.Queue[SearchNode]()
    def shortestPath(source: Node, destination: Node): Int = {
      // Zero out visited
      for (i <- 0 until visited.length) {
        for (j <- 0 until visited(0).length) {
          visited(i)(j) = false
        }
      }

      // empty Queue
      while (q.nonEmpty) q.dequeue()

      println(s"Looking for sp from $source to $destination")

      visited(source.i)(source.j) = true
      q.enqueue(SearchNode(source.i, source.j, 0))

      var dist = -1
      var cont = true

      while (q.nonEmpty && cont) {
        val gNode = q.dequeue()

        val newDist = gNode.dist + 1

        diffs.foreach { case (di, dj) =>
          val ni = gNode.i + di
          val nj = gNode.j + dj
          if (
            inBounds(ni, visited.length) &&
              inBounds(nj, visited(0).length) &&
              !visited(ni)(nj) &&
              forest(ni)(nj) != 0
          ) {
            visited(gNode.i)(gNode.j) = true

            if (ni == destination.i && nj == destination.j) {
              dist = newDist
              cont = false
            } else {
              q.enqueue(SearchNode(ni, nj, newDist))
            }
          }
        }

      }

      dist
    }

    val trees = mutable.ArrayBuffer[Tree]()

    for (i <- 0 until forest.length) {
      for (j <- 0 until forest(0).length) {
        if (forest(i)(j) > 1) trees.append(Tree(i, j, forest(i)(j)))
      }
    }

    val nodeOrder = trees.sortBy(_.height).map { case Tree(i, j, _) => Node(i, j) }

    var ans = 0
    var curr = Node(0, 0)
    var cont = true
    for (i <- 0 until nodeOrder.length if cont) {
      val next = nodeOrder(i)
      val sp = if (curr == next) 0 else shortestPath(curr, next)
      if (sp != -1) {
        ans += sp
        curr = next
      } else {
        ans = -1
        cont = false
      }
    }

    ans
  }
}
