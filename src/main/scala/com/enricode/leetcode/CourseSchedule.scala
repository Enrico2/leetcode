package com.enricode.leetcode

import com.enricode.util.{Graph, LeetcodeApp}
import scala.collection.mutable

/**
  * https://leetcode.com/problems/course-schedule/
  */
object CourseSchedule extends LeetcodeApp {

  override def run(): Unit = {
    println(
      canFinish2(
        8,
        Array(Array(1, 0), Array(2, 6), Array(1, 7), Array(6, 4), Array(7, 0), Array(0, 5)))
    )
  }

  def canFinish2(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    var ans = true

    if (prerequisites.length > 0) {
      val adj = Array.fill[mutable.ListBuffer[Int]](numCourses)(mutable.ListBuffer[Int]())
      val indegree = Array.fill[Int](numCourses)(0)

      for (p <- prerequisites) {
        adj(p(1)) += p(0)
        indegree(p(0)) += 1
      }


      for (_ <- 0 until numCourses if ans) {
        indegree.indices.find { i => indegree(i) == 0 } match {
          case None => ans = false
          case Some(j) =>
            indegree(j) = -1
            for (child <- adj(j)) {
              indegree(child) -= 1
            }
        }
      }
    }

    ans
  }


  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
    val graph = new Graph[Int](prerequisites, true)
    val good = Array.fill(numCourses)(false)

    good.indices.forall { i =>
      if (graph.vertices.get(i).isEmpty) true
      else if (good(i)) true
      else {
        val visited = mutable.HashSet[Int]()
        val s = mutable.Stack[Int]()
        s.push(graph.vertices(i).data)

        var continue = true

        while (s.nonEmpty && continue) {
          val k = s.pop()
          if (!good(k)) {
            graph.vertices.get(k) match {
              case Some(kNode) =>
                if (visited(kNode.data)) {
                  continue = false
                } else {
                  kNode.edges.foreach { edge =>
                    if (!good(edge.n2.data)) {
                      s.push(edge.n2.data)
                    }
                  }
                }

                visited += kNode.data
              case None =>
                good(k) = true
            }
          }
        }

        if (continue) {
          good(i) = continue
        }

        continue
      }
    }
  }
}