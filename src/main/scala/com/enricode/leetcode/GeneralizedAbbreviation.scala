package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/generalized-abbreviation/description/
  */
object GeneralizedAbbreviation extends LeetcodeApp {

  override def run(): Unit = {
    val got = generateAbbreviations("approximations")
    val expected = Seq("word", "1ord", "w1rd", "wo1d", "wor1", "2rd", "w2d", "wo2", "1o1d", "1or1", "w1r1", "1o2", "2r1", "3d", "w3", "4")

    println(s"got :$got, diff: ${got.diff(expected)}, diff: ${expected.diff(got)}")
  }

  private[this] def children(word: String): List[String] = {
    (1 to word.length).flatMap { replacer =>
      (0 until word.length - replacer + 1).flatMap { start =>
        val prefix = word.take(start)
        val suffix = word.drop(start + replacer)
        val w = prefix + replacer + suffix
        val grandkids = suffix.drop(1)
        val strings = children(grandkids).map { end =>
          prefix + replacer + suffix.take(1) + end
        }

        List(w) ++ strings
      }
    }.toList
  }

  def generateAbbreviationsRecursive(word: String): List[String] = {
    List(word) ++ children(word)
  }


  case class Node(w: String, start: Int)

  def generateAbbreviations(word: String): List[String] = {
    val l = mutable.ListBuffer[String](word)
    val s = mutable.Stack[Node]()
    s.push(Node(word, 0))

    while (s.nonEmpty) {
      val Node(w, start) = s.pop()

      (1 to (w.length - start)).flatMap { replacer =>
        (0 to (w.length - start - replacer)).map { i =>
          val prefix = w.take(start + i)
          val suffix = w.drop(start + i + replacer)
          val res = prefix + replacer + suffix
          l += res

          val newStart = prefix.length + replacer.toString.length + 1
          if (newStart < w.length) s.push(Node(res, newStart))
        }
      }.toList
    }

    l.toList
  }
}
