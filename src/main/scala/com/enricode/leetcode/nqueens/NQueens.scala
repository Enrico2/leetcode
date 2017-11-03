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
    if (a.map(findQueen(_)).sum == (0 until n).sum) {
      var rep = true
      for (i <- 0 until n; j <- 0 until n if rep) {
        if (a(i)(j)) {
          var x = 0
          var y = 0

          x = i-1; y = j-1; while (rep && x >= 0 && y >= 0) { if (a(x)(y)) rep = false; x -= 1; y -= 1 }
          x = i-1; y = j+1; while (rep && x >= 0 && y < n ) { if (a(x)(y)) rep = false; x -= 1; y += 1 }
          x = i+1; y = j-1; while (rep && x < n  && y >= 0) { if (a(x)(y)) rep = false; x += 1; y -= 1 }
          x = i+1; y = j+1; while (rep && x < n  && y < n ) { if (a(x)(y)) rep = false; x += 1; y += 1 }
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

  private[this] def solve(a: Seq[mutable.Seq[Boolean]], row: Int, n: Int): Unit = {
    if (row == n) {
      if (isSolution(a)) solutions.add(a)
    } else {
      while (moveQueenPlus1(a(row))) {
        solve(a, row + 1, n)
      }
    }
  }

  def solveNQueens(n: Int): List[List[String]] = {
    val a = Seq.fill(n)(mutable.Seq.fill(n)(false))
    solve(a, 0, n)

    solutions.toResponse()
  }

  val start = System.currentTimeMillis()
  println(s"${solveNQueens(8)}")
  println(s"time: ${System.currentTimeMillis() - start}ms")
}
