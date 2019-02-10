package com.enricode.util

import scala.collection.mutable

trait State

case class StarState(c: Char) extends State

case class CharState(c: Char) extends State

// Example, Regex processor, support chars; .; +; *;
class RegexProcessor(regex: String) {
  // 0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz

  private[this] val states = mutable.ListBuffer[State]()

  def add(state: State): Unit = {
    states.append(state)
  }

  private[this] def inRange(c: Char, first: Char, last: Char): Boolean =
    first <= c && c <= last

  var i = 0
  while (i < regex.length) {
    regex.charAt(i) match {
      case c if inRange(c, '0', '9') || inRange(c, 'A', 'Z') || inRange(c, 'a', 'z') || c == '.' =>
        if (i < regex.length - 1) {
          regex.charAt(i + 1) match {
            case '+' =>
              add(CharState(c))
              add(StarState(c))
              i += 1
            case '*' =>
              add(StarState(c))
              i += 1
            case _ =>
              add(CharState(c))
          }
        } else {
          add(CharState(c))
        }

      case _ => throw new Exception(s"Bad Regex: $regex")
    }
    i += 1
  }

  def apply(str: String) = {
    var head = states.toList
    var i = 0
    var flag = true
    while (i < str.length && head.nonEmpty && flag) {
      val char = str.charAt(i)
      head.head match {
        case CharState(c) if c == '.' =>
          i += 1
          head = head.tail
        case CharState(c) if c == char =>
          i += 1
          head = head.tail
        case StarState(c) =>
        // cont from here.

        case CharState(c) if c != char =>
          flag = false
      }
    }

    flag && i == str.length && head.forall {
      case StarState(_) => true
      case _ => false
    }
  }
}

object Main extends LeetcodeApp {
  override def run(): Unit = {
    val rp = new RegexProcessor("a+b*..*f+")
    println(rp("ab8asdffffff"))
  }
}