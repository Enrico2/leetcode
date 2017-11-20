package com.enricode.leetcode

import java.util
import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/sliding-window-maximum/description/
  */
object SlidingWindowMaximum extends LeetcodeApp {
  override def run(): Unit = {
    println(maxSlidingWindow(Array(1,3,1,2,0,5), 3).toList)
  }

  def maxSlidingWindow(a: Array[Int], k: Int): Array[Int] = {
    val n = a.length
    if (n == 0) return Array.empty[Int]

    val leftMax = Array.ofDim[Int](n)
    val rightMax = Array.ofDim[Int](n)

    var maxSoFar = Int.MinValue
    leftMax.indices.foreach { i =>
      if (i % k == 0) {
        maxSoFar = Int.MinValue
      }
      maxSoFar = math.max(a(i), maxSoFar)
      leftMax(i) = maxSoFar
    }

    maxSoFar = Int.MinValue
    rightMax.indices.reverse.foreach { i =>
      if (i % k == k-1) {
        maxSoFar = Int.MinValue
      }
      maxSoFar = math.max(a(i), maxSoFar)
      rightMax(i) = maxSoFar
    }

    val d = Array.ofDim[Int](n - k + 1)
    d.indices.foreach { i =>
      d(i) = math.max(rightMax(i), leftMax(i+k-1))
    }
    d
  }


  def maxSlidingWindowSlow(a: Array[Int], k: Int): Array[Int] = {
    val n = a.length
    if (n == 0) return Array.empty[Int]

    val deque = new util.ArrayDeque[Int]()
    val ans = Array.ofDim[Int](n - k + 1)

    for (i <- 0 until a.length) {
      while (!deque.isEmpty && deque.peek() < i - k + 1) {
        deque.poll()
      }

      while (!deque.isEmpty && a(i) > a(deque.peekLast())) {
        deque.pollLast()
      }

      deque.offer(i)
      if (i >= k - 1) ans(i-k+1) = a(deque.peekFirst())
    }

    ans
  }
}
