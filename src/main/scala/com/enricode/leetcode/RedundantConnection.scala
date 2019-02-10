package com.enricode.leetcode

import com.enricode.util.{Dsu, LeetcodeApp}

object RedundantConnection extends LeetcodeApp {

  override def run(): Unit = {
    val Array(a, b) = findRedundantConnection(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 4), Array(1, 5)))
    println(s"($a, $b)")
  }

  def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] = {
    val dsu = new Dsu[Int]
    edges.find { case Array(a, b) => !dsu.union(a, b) }.getOrElse(throw new IllegalArgumentException(""))
  }
}