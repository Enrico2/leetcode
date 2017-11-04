package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/n-queens
  * Leet code has some weird rules about global variables, so this code isn't ideal.
  */
object NQueens extends LeetcodeApp {
  var solutions: Solutions = null

  class Solutions() {
    private[this] val solutions = mutable.ListBuffer[Seq[String]]()

    def toResponse(): List[List[String]] = solutions.map(_.toList).toList

    def add(a: Seq[mutable.Seq[Boolean]]): Unit = solutions.append(
      a.map { bools =>
        bools.map { b => if (b) 'Q' else '.' }.mkString("")
      }
    )
  }

  private[this] def findQueen(a: Seq[Boolean]): Int = {
    var r = -1
    for (i <- 0 until a.length) {
      if (a(i)) r = i
    }
    r
  }

  private[this] def isSolution(a: Seq[mutable.Seq[Boolean]]): Boolean = {
    val n = a.length

    var bad = false
    for (i <- 0 until n) {
      var s = 0
      for (j <- 0 until n) {
        if (a(j)(i)) s += 1
      }
      if (s > 1) bad = true
    }

    if (!bad) {
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
        if (isSolution(a)) solve(a, row + 1)
      }
    }
  }

  def solveNQueens(n: Int): List[List[String]] = {
    if (n == 1) List(List("Q"))
    else if (n < 4) Nil
    else {
      solutions = new Solutions()
      solve(Seq.fill(n)(mutable.Seq.fill(n)(false)), 0)

      solutions.toResponse()
    }
  }

  override def run(): Unit = {
    val solves = solveNQueens(5)
    println(s"number of solutions: ${solves.size}")
    println(s"$solves")
  }
}
