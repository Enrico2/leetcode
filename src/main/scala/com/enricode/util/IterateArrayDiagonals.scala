package com.enricode.util

object IterateArrayDiagonals {
  def apply[T](a: Array[Array[T]])(f: T => Unit): Unit = {
    val n = a.length
    val m = a(0).length
    val totalDiags = n + m - 1

    for (k <- 0 until totalDiags) {
      for (j <- 0 to k) {
        val i = k - j
        if (i < n && j < m) {
          // println(s"($i, $j)")
          f(a(i)(j))
        }
      }
      // println("")
    }
  }
}
