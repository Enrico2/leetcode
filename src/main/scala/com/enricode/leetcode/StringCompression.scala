package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

object StringCompression extends LeetcodeApp {

  override def run(): Unit = {
//    val a2 = mutable.ArrayBuffer('a', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b').toArray
//    val a2 = mutable.ArrayBuffer('a','a','b','b','c','c','c').toArray
    val a2 = mutable.ArrayBuffer('a','a','a','a','a','a','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','b','c','c','c','c','c','c','c','c','c','c','c','c','c','c').toArray
    val l = compress(a2)
    println(a2.toList.take(l))
  }

  def compress(chars: Array[Char]): Int = {
    if (chars.length < 2) return chars.length

    var curr = 0
    var write = 0

    for (i <- 0 until chars.length) {
      if (i+1 == chars.length || chars(i+1) != chars(i)) {
        chars(write) = chars(curr)
        write += 1
        val num = (i - curr + 1)
        if (num > 1) {
          num.toString.toCharArray.foreach { c =>
            chars(write) = c
            write += 1
          }
        }

        curr = i+1
      }
    }
    write

  }
}
