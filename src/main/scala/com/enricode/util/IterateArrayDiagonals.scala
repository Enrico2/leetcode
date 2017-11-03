package com.enricode.util

object IterateArrayDiagonals extends App {
  def apply[T](a: Seq[Seq[T]], rtl: Boolean = true)(f: T => Unit): Unit = {
    val n = a.length
    val m = a(0).length
    val totalDiags = n + m - 1

    for (k <- 0 until totalDiags) {
      for (j <- 0 to k) {
        val i = k - j
        if (i < n && j < m) {
          val (x, y) = if (rtl) (i, j) else (j, n-1-i)

          println(s"($x, $y)")
          f(a(i)(j))
        }
      }
      println("")
    }
  }

  apply(Seq.fill(8)(Seq.fill(8)(0)), false)(_ => ())
}
