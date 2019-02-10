package com.enricode.leetcode

import com.enricode.util.{ArrayPermutations, LeetcodeApp}
import scala.collection.mutable

object BruteForceSlowNQueens extends LeetcodeApp {
  val Q = 'Q'
  val D = '.'

  private[this] def makeStream(n: Int): Stream[Seq[Int]] = {
    ArrayPermutations(1 to n).map(_.toList)
  }

  /**
    * We only need to check diagonals because it's impossible to generate a solution
    * that has queens in the same row/column.
    */
  private[this] def isSolution(ints: Seq[Int]): Boolean = {
    val n = ints.length
    val a = ints.map { i =>
      val seq = mutable.Seq(Seq.fill(n)(false): _*)
      seq(i - 1) = true
      Seq(seq: _*)
    }

    var rep = true

    for (i <- 0 until n; j <- 0 until n) {
      if (a(i)(j)) {
        var x = 0
        var y = 0

        x = i-1; y = j-1; while (x >= 0 && y >= 0) { if (a(x)(y)) rep = false; x -= 1; y -= 1 }
        x = i-1; y = j+1; while (x >= 0 && y < n ) { if (a(x)(y)) rep = false; x -= 1; y += 1 }
        x = i+1; y = j-1; while (x < n  && y >= 0) { if (a(x)(y)) rep = false; x += 1; y -= 1 }
        x = i+1; y = j+1; while (x < n  && y < n ) { if (a(x)(y)) rep = false; x += 1; y += 1 }

      }
    }

    rep
  }

  private[this] def toSolution(ints: Seq[Int]): List[String] = {
    ints.toList.map { i =>
      (1 to ints.length).map { j => if (i == j) Q else D }.mkString("")
    }
  }

  def solveNQueens(n: Int): List[List[String]] = {
    val solutions = scala.collection.mutable.ListBuffer[List[String]]()
    val stream = makeStream(n).iterator
    while (stream.nonEmpty) {
      val sol = stream.next()
      if (isSolution(sol)) {
        solutions.append(toSolution(sol))
      }
    }
    solutions.toList
  }

  override def run(): Unit = println(s"${solveNQueens(4)}")
}