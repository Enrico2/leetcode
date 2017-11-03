package com.enricode.leetcode.nqueens

import scala.collection.mutable

object NQueens extends App {
  val Q = 'Q'
  val D = '.'

  type Solution = Seq[String]

  class Solutions() {
    private[this] val solutions = mutable.ListBuffer[Solution]()

    def toResponse(): List[List[String]] = solutions.map(_.toList).toList

    def add(a: Seq[mutable.Seq[Boolean]]): Unit = solutions.append(
      a.map { bools =>
        bools.map { b => if (b) Q else D }.mkString("")
      }
    )
  }

  val solutions: Solutions = new Solutions()

  private[this] def findQueen(a: Seq[Boolean]): Int = {
    var r = -1
    for (i <- 0 until a.length) {
      if (a(i)) r = i
    }
    r
  }

  private[this] def isSolution(a: Seq[mutable.Seq[Boolean]]): Boolean = {
    val n = a.length
    if (a.map(findQueen(_)).sorted == (0 until n)) {
      var rep = true

      for (k <- 0 until (2 * n) - 1 if rep) {
        var rtl = 0
        var ltr = 0

        for (j <- 0 to k) {
          val i = k - j
          if (i < n && j < n) {
            if (a(i)(j)) rtl += 1
            if (a(j)(n - 1 - i)) ltr += 1
          }
        }

        if (rtl > 1 || ltr > 1) {
          rep = false
        }
      }

      rep

    } else {
      false
    }
  }

  private[this] def moveQueenPlus1(a: mutable.Seq[Boolean]): Boolean = {
    val q = findQueen(a)
    if (q == -1) {
      a(0) = true
      true
    } else if (q == a.length - 1) {
      a(q) = false
      false
    } else {
      a(q) = false
      a(q + 1) = true
      true
    }
  }

  private[this] def solve(a: Seq[mutable.Seq[Boolean]], row: Int): Unit = {
    if (row == a.length) {
      if (isSolution(a)) solutions.add(a)
    } else {
      while (moveQueenPlus1(a(row))) {
        solve(a, row + 1)
      }
    }
  }

  def solveNQueens(n: Int): List[List[String]] = {
    val a = Seq.fill(n)(mutable.Seq.fill(n)(false))
    solve(a, 0)

    solutions.toResponse()
  }

  val start = System.currentTimeMillis()
  val solves = solveNQueens(8)
  println(s"number of solutions: ${solves.size}")
  println(s"$solves")
  println(s"time: ${System.currentTimeMillis() - start}ms")
}
