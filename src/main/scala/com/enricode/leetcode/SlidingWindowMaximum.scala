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
