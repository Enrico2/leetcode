package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

object SuperWashingMachines extends LeetcodeApp {
  private[this] def pp(a: Array[Int]) = a.mkString("[", ",", "]")

  override def run(): Unit = {
    println(findMinMoves(Array(0, 0, 8, 4)))
  }

  def findMinMoves(machines: Array[Int]): Int = {
    val n = machines.length
    val total = machines.sum
    if (total % n != 0) return -1
    val avg = total / n

    machines.foldLeft((Int.MinValue, 0)) { case ((max, count), load) =>
      val accumulated = count + load - avg
      (Seq(max, math.abs(accumulated), load - avg).max, accumulated)
    }._1
  }
}
