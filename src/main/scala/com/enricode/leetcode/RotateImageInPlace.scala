package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/rotate-image/description/
  */
object RotateImageInPlace extends LeetcodeApp {

  override def run(): Unit = {
    val a = Array(
      Array(15, 13, 2, 5, 1),
      Array(14, 3, 4, 1, 1),
      Array(12, 6, 8, 9, 1),
      Array(16, 7, 10, 11, 1),
      Array(5, 5, 5, 5, 5),
    )

    rotate(a)

    a.toList.map(_.toList).foreach(println)
  }

  def rotate(matrix: Array[Array[Int]]): Unit = {
    val n = matrix.length - 1

    def set(ij: (Int, Int), v: Int) = matrix(ij._1)(ij._2) = v

    def get(ij: (Int, Int)): Int = matrix(ij._1)(ij._2)

    val loops = matrix.length / 2
    for (i <- 0 until loops) {
      for (j <- i to (n - i - 1)) {
        val top = (i, j)
        val right = (j, n - i)
        val bottom = (n - i, n - j)
        val left = (n - j, i)

        val temp = get(top)
        set(top, get(left))
        set(left, get(bottom))
        set(bottom, get(right))
        set(right, temp)
      }
    }
  }
}
