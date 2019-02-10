package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/two-sum/description/
  */
object TwoSum extends LeetcodeApp {

  override def run(): Unit = println(twoSum(Array(1, 4, 7, 23), 24).toList == Array(0, 3).toList)

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val m = mutable.HashMap[Int, Int]()
    var ans = Array[Int]()
    for (i <- 0 until nums.length) {
      val v = nums(i)
      m.get(target - v) match {
        case Some(j) => ans = Array(j, i)
        case _ => ()
      }
      m.put(v, i)
    }
    ans
  }
}
