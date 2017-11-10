package com.enricode.util

object Util extends App {
  def inBounds(i: Int, max: Int, min: Int = 0): Boolean = i >= min && i < max

  def toLists(str: String) = {
    str
      .replaceAllLiterally("[", "List(")
      .replaceAllLiterally("]", ")")
  }

  println(toLists("[[4,10,15,24,26],[0,9,12,20],[5,18,22,30]]"))
}
