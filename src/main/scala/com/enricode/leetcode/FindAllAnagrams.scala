package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/find-all-anagrams-in-a-string/description/
  */
object FindAllAnagrams extends LeetcodeApp {
  override def run(): Unit = {
    println(findAnagrams("cbaebabacd", "abc"))
  }

  def findAnagrams(s: String, p: String): List[Int] = {
    val hash = new Array[Int](256)
    p.foreach(hash(_) += 1)

    var left = 0
    var right = 0
    var count = p.length
    var ans: List[Int] = Nil

    while (right < s.length) {
      if (hash(s.charAt(right)) >= 1) count -= 1
      hash(s.charAt(right)) -= 1
      right += 1

      if (count == 0) ans = ans :+ left

      if (right - left == p.length) {
        if (hash(s.charAt(left)) >= 0) count += 1
        hash(s.charAt(left)) += 1
        left += 1
      }
    }

    ans
  }
}
