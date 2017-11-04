package com.enricode.leetcode

import com.enricode.util.LeetcodeApp


/**
  * https://leetcode.com/problems/best-meeting-point
  */
object BestMeetingPoint extends LeetcodeApp {
  import scala.collection.mutable

  override def run(): Unit =
    println(minTotalDistance(Array(Array(1,0,0,0,1),Array(0,0,0,0,0),Array(0,0,1,0,0))))

  def collect(grid: Array[Array[Int]], rows: Boolean) = {
    def choose[T](r: => T, c: => T) = if (rows) r else c

    val ints = scala.collection.mutable.ArrayBuffer[Int]()
    val rc = (grid.length, grid(0).length)
    val (n, m) = choose(rc, rc.swap)

    for (i <- 0 until n) {
      for (j <- 0 until m) {
        val value = choose(grid(i)(j), grid(j)(i))
        if (value == 1) {
          ints.append(i)
        }
      }
    }
    ints
  }

  def minDistance1D(ints: mutable.ArrayBuffer[Int]) = {
    var distance = 0
    var i = 0
    var j = ints.length - 1
    while (i < j) {
      distance += ints(j) - ints(i)
      i += 1
      j -= 1
    }
    distance
  }

  def minTotalDistance(grid: Array[Array[Int]]): Int = {
    val rows = collect(grid, true)
    val cols = collect(grid, false)
    minDistance1D(rows) + minDistance1D(cols)
  }

  /**
    * Slow, trivial, solution, O(m*n*P) where P is number of points, could be n*m so worst case is O(n^2*m^2)
    *
    * @param grid
    * @return
    */
  def SlowMinTotalDistance(grid: Array[Array[Int]]): Int = {
    case class Point(i: Int, j: Int) {
      def d(x: Int, y: Int) = math.abs(x - i) + math.abs(y - j)
    }

    val n = grid.length
    val m = grid(0).length

    val points = mutable.ListBuffer[Point]()

    // Find points
    for (i <- 0 until n) {
      for (j <- 0 until m) {
        if (grid(i)(j) == 1) points.append(Point(i, j))
      }
    }

    var min = Int.MaxValue
    for (i <- 0 until n) {
      for (j <- 0 until m) {
        val maybeMin = points.map { p => p.d(i, j) }.sum
        if (maybeMin <= min)
          min = maybeMin
      }
    }

    min
  }
}
