package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/longest-palindromic-substring/description/
  */
object LongestPalindrome extends LeetcodeApp {
  override def run(): Unit = {
    println(longestPalindrome("asdftattarrattatfasdf"))
  }

  def longestPalindrome(s: String): String = {
    val n = s.length
    val memo = Array.ofDim[Boolean](n, n)

    var maxLength = Int.MinValue
    var ans = ""

    def updateMax(i: Int, j: Int, l: Int) = {
      if (l > maxLength) {
        maxLength = l
        ans = s.substring(i, j+1)
      }
    }

    for (l <- 1 to n) {
      for (i <- 0 to n - 1) {
        val j = i + l - 1
        l match {
          case 1 =>
            memo(i)(j) = true
            if (memo(i)(j)) updateMax(i, j, l)

          case 2 if (j < n) =>
            memo(i)(j) = s(i) == s(j)
            if (memo(i)(j)) updateMax(i, j, l)

          case _ if (j < n) =>
            memo(i)(j) = memo(i + 1)(j - 1) && s(i) == s(j)
            memo(i)(j)
            if (memo(i)(j)) updateMax(i, j, l)

          case _ => false
        }
      }
    }

    ans
  }
}
