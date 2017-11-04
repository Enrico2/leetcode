package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/string-to-integer-atoi/description/
  */
object StringToInteger extends LeetcodeApp {
  override def run(): Unit = {
    println(myAtoi("9223372036854775809"))
  }


  def myAtoi(str: String): Int = {
    def toDigit(c: Char) = (c - '0').toLong


    val regex = "\\s*([+-]?)(\\d+).*".r

    str match {
      case regex(sign, num) =>
        val n = num.tail.foldLeft(toDigit(num.head)) { (s, c) =>
          s * 10 + toDigit(c)
        }

        val neg = (sign == "-")
        val r = if (neg) -n else n
        if (Int.MaxValue.toString.length < num.length) {
          if (neg) Int.MinValue
          else Int.MaxValue
        } else {
          math.max(math.min(r, Int.MaxValue), Int.MinValue).toInt
        }

      case _ => 0
    }
  }
}
