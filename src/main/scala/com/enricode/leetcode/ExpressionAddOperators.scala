package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable.ListBuffer

/**
  * https://leetcode.com/problems/expression-add-operators/description/
  */
object ExpressionAddOperators extends LeetcodeApp {


  override def run(): Unit = {
    println(addOperators("105", 5))
  }


  def addOperators(num: String, target: Int): List[String] = {
    val ans = ListBuffer[String]()

    def add(path: String, pos: Int, eval: Long, mult: Long): Unit = {
      if (pos == num.length) {
        if (eval == target) ans.append(path)
      } else {
        var flag = true
        for (i <- pos until num.length if flag) {
          if (i > pos && num.charAt(pos) == '0') flag = false
          else {
            val curr = num.substring(pos, i + 1).toLong
            if (pos == 0) {
              add(path + curr, i + 1, curr, curr)
            } else {
              add(s"$path+$curr", i + 1, eval + curr, curr)
              add(s"$path-$curr", i + 1, eval - curr, -curr)
              add(s"$path*$curr", i + 1, eval - mult + mult * curr, mult * curr)
            }
          }
        }
      }
    }

    add("", 0, 0, 0)

    ans.toList
  }
}
