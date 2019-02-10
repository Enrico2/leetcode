package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/valid-parentheses/description/
  */
object ValidParenthesis extends LeetcodeApp {

  override def run(): Unit = {
    val ss = Seq("]", "(", "()", "({)}", "({[][]()}({})[])") // false, true, false, true
    ss.foreach(s => println(isValid(s)))
  }

  def isValid(str: String): Boolean = {
    val stack = mutable.Stack[Char]() // ()

    def flip(c: Char) = c match {
      case ')' => '('
      case ']' => '['
      case '}' => '{'
    }

    val r = str.foldLeft(true) { (b, c) =>
      if (!b) b else {
        c match {
          case ')' | ']' | '}' =>
            if (stack.isEmpty) false else stack.pop() == flip(c)
          case c =>
            stack.push(c)
            true
        }
      }
    }

    r && stack.isEmpty
  }
}
