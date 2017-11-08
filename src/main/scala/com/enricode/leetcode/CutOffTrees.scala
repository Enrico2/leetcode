package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/cut-off-trees-for-golf-event/description/
  */
object CutOffTrees extends LeetcodeApp {

  import com.enricode.util.RanArrays.inBounds

  override def run(): Unit = {
    val ans = cutOffTree(
      List(
        List(2, 3, 4, 5),
        List(0, 0, 6, 7),
        List(10, 9, 8, 0)
      )
    )

    println(s"$ans")

  }

  import scala.collection.mutable

  def cutOffTree(forest: List[List[Int]]): Int = {
    case class Tree(i: Int, j: Int, height: Int)
    case class Node(i: Int, j: Int)

    val trees = mutable.ArrayBuffer[Tree]()

    for (i <- 0 until forest.length) {
      for (j <- 0 until forest(0).length) {
        if (forest(i)(j) > 1) trees.append(Tree(i, j, forest(i)(j)))
      }
    }

    val nodeOrder = trees.sortBy(_.height).map { case Tree(i, j, _) => Node(i, j) }

    def shortestPath(source: Node, destination: Node): Int = {
      val visited = Array.ofDim[Boolean](forest.length, forest(0).length)
      visited(source.i)(source.j) = true
      case class GNode(node: Node, dist: Int)
      val q = new mutable.Queue[GNode]()
      q.enqueue(GNode(source, 0))

      var dist = -1
      while (q.nonEmpty) {
        val gNode = q.dequeue()
        visited(gNode.node.i)(gNode.node.j) = true

        if (gNode.node == destination) {
          dist = gNode.dist
        } else {
          val Node(i, j) = gNode.node
          val newDist = gNode.dist + 1

          Seq((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)).foreach { case (ni, nj) =>
            if (
              inBounds(ni, visited.length) &&
                inBounds(nj, visited(0).length) &&
                !visited(ni)(nj) &&
                forest(ni)(nj) != 0
            ) q.enqueue(GNode(Node(ni, nj), newDist))
          }
        }
      }

      dist
    }

    var ans = 0
    var curr = Node(0, 0)
    var cont = true
    for (i <- 0 until nodeOrder.length if cont) {
      val next = nodeOrder(i)
      val sp = shortestPath(curr, next)
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
