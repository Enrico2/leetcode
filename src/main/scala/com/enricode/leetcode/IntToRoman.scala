package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/integer-to-roman/description/
  */
object IntToRoman extends LeetcodeApp {

  override def run(): Unit = {
    Seq(129).foreach { i =>
      println(intToRoman(i))
    }
  }

  def intToRoman(num: Int): String = {
    val nums = mutable.LinkedHashMap(
      "M" -> 1000,
      "CM" -> 900,
      "D" -> 500,
      "CD" -> 400,
      "C" -> 100,
      "XC" -> 90,
      "L" -> 50,
      "XL" -> 40,
      "X" -> 10,
      "IX" -> 9,
      "V" -> 5,
      "IV" -> 4,
      "I" -> 1,
    )

    def rec(n: Int): String = {
      if (n == 0) return ""

      nums.collectFirst {
        case (rn, v) if n - v >= 0 => rn + rec(n - v)
      }.get
    }

    rec(num)
  }
}
