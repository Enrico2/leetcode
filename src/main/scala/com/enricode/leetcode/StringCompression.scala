package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

object StringCompression extends LeetcodeApp {

  override def run(): Unit = {
    val a = mutable.ArrayBuffer('a', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b').toArray
    val a2 = mutable.ArrayBuffer('a','a','b','b','c','c','c').toArray
//    val a2 = mutable.ArrayBuffer('a','a','a','a','a','a','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','c','c','c','c','c','c','c','c','c','c','c','c','c','c').toArray
    val l = compress(a2)
    println(a2.toList.take(l))
  }

  def compress(chars: Array[Char]): Int = {
    if (chars.length < 2) return chars.length

    var curr = chars(chars.length - 1)
    var l = chars.length
    var count = 1
    var prevLetter = chars.length

    def move(from: Int, to: Int) = {
      if (from < chars.length) {
        for (i <- 0 until (l-from)) {
          chars(to + i) = chars(from + i)
        }
      }
    }

    for (i <- chars.length - 2 to -1 by -1) {
      if (i > -1 && chars(i) == curr) {
        count += 1
      } else {
        if (count > 1) {
          val j = i+1
          val str = (curr + count.toString).toCharArray
          for (k <- j until j+str.length) {
            chars(k) = str(k-j)
          }
          move(prevLetter, j+str.length)
          prevLetter = j
          l -= (count-str.length)
        }

        count = 1
        if (i > -1) curr = chars(i)
      }
    }

    l
  }
}
