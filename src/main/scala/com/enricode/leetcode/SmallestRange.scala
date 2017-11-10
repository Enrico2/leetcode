package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/smallest-range/description/
  */
object SmallestRange extends LeetcodeApp {
  override def run(): Unit = {
    val nums = List(List(4,10,15,24,26),List(0,9,12,20),List(5,18,22,30))
    println(smallestRange(nums).toList)
  }

  def smallestRange(nums: List[List[Int]]): Array[Int] = {
    implicit val ord = new Ordering[List[Int]] {
      override def compare(x: List[Int], y: List[Int]) = x.head - y.head
    }

    val pqMin = new mutable.PriorityQueue[List[Int]]().reverse
    nums.foreach(pqMin.enqueue(_))

    var minRange = (0, Int.MaxValue)
    var max = nums.map(_.head).max

    var cont = true

    while (cont) {
      val min = pqMin.dequeue()
      if (minRange._2 - minRange._1 > max-min.head) minRange = (min.head, max)

      val tail = min.tail
      if (tail.isEmpty) {
        cont = false
      } else {
        max = math.max(max, tail.head)
        pqMin.enqueue(tail)
      }
    }

    Array(minRange._1, minRange._2)
  }

  /*


                             |     |
  List 1:     X     X    X        X X       [4, 10, 15, 24,26], 24 is in range [20,24].
  List 2: X        X  X       X             [0, 9, 12, 20], 20 is in range [20,24].
  List 3:      X            X   X       X   [5, 18, 22, 30], 22 is in range [20,24].
          0    5    0    5    0    5    0
                    1    1    2    2    3
   */
}
