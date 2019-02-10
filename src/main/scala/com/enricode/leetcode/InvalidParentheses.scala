package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/remove-invalid-parentheses
  */
object InvalidParentheses extends LeetcodeApp {
  override def run(): Unit = {
    val ins = Seq("()())()", "(())()", "()()()", "(a)())()", "(a())()", ")(")

    ins.map(removeInvalidParentheses) foreach println
  }

  import scala.collection.mutable.ListBuffer

  private[this] def isValid(str: String): Boolean = {
    var s = List[Char]()

    def push(c: Char) = {
      s = c +: s
    }

    def pop = {
      s = s.tail
    }

    val ans = str.foldLeft(true) {
      case (false, _) => false
      case (_, c) if c == '(' => push(c); true
      case (_, c) if c == ')' => if (s.isEmpty) {
        false
      } else {
        pop
        true
      }
      case _ => true
    }
    ans && s.isEmpty
  }

  private[this] def remove(pointers: Array[Int], str: String): String = {
    str.zipWithIndex.collect {
      case (c, i) if !pointers.contains(i) => c
    }.mkString("")
  }

  private[this] def tryRemoving(removals: Int, str: String): List[String] = {
    if (removals == 0) return (if (isValid(str)) List(str) else Nil)

    val pointers = (0 until removals).toArray

    def updatePointers(): Boolean = {
      def next(i: Int): Boolean = {
        var changed = false
        val p = pointers(i)
        val max = str.length - (pointers.length - i)

        if (p == max) {
          if (i != 0) next(i - 1) else false
        } else {
          pointers(i) = p + 1
          changed = true

          if (i < pointers.length - 1) {
            for (j <- i + 1 until pointers.length) {
              pointers(j) = pointers(j - 1) + 1
            }
          }
          changed
        }
      }

      next(pointers.length - 1)
    }

    val ans = ListBuffer[String]()
    var cont = true
    while (cont) {
      val strToTest = remove(pointers, str)
      if (isValid(strToTest)) ans.append(strToTest)
      cont = updatePointers()
    }
    ans.toList
  }

  def removeInvalidParentheses(str: String): List[String] = {
    var cont = true
    var ans = List[String]("")
    for (removals <- 0 to str.length if cont) {
      val r = tryRemoving(removals, str)
      if (r.nonEmpty) {
        cont = false
        ans = r
      }
    }

    ans.distinct
  }
}
