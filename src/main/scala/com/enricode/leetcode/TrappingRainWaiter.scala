package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/trapping-rain-water/description/
  */
object TrappingRainWaiter extends LeetcodeApp {

  override def run(): Unit = println(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))

  def trap(height: Array[Int]): Int = {
    val n = height.length
    if (n < 3) 0 else {
      var total = 0

      val lefts = new mutable.ArraySeq[Int](n)
      val rights = new mutable.ArraySeq[Int](n)

      lefts(0) = height(0)
      for (i <- 1 until n) {
        lefts(i) = math.max(height(i), lefts(i - 1))
      }

      rights(n-1) = height(n-1)
      for (i <- n-2 to 0 by -1) {
        rights(i) = math.max(height(i), rights(i + 1))
      }

      for (i <- 0 until n) {
        total += math.min(lefts(i), rights(i)) - height(i)
      }

      total
    }
  }
}
