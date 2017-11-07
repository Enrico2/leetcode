package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

class ValidWordAbbr(dict: Array[String]) {

  sealed trait MySet
  case class Unique(set: Set[String]) extends MySet
  case object NonUnique extends MySet

  private[this] def abbrv(s: String): String = {
    if (s.length <= 2) s
    else s(0) + s.substring(1, s.length-1).length.toString + s(s.length-1)
  }
  private[this] val m: Map[String, MySet] = dict.groupBy(abbrv).map { case (k, words) =>
    val s = words.toSet
    k -> (if (s.size > 1) NonUnique else Unique(s))
  }

  def isUnique(word: String): Boolean = {
    m.get(abbrv(word)) match {
      case None => true
      case Some(NonUnique) => false
      case Some(Unique(s)) => s.contains(word)
    }
  }
}

/**
  * https://leetcode.com/problems/unique-word-abbreviation
  */
object UniqueWordAbbreviation extends LeetcodeApp {
  override def run(): Unit = ???
}
