package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/maximum-frequency-stack/
  */
class FreqStack() {

  case class Entry(v: Int, freq: Int, order: Int)

  private[this] val ordering = new Ordering[Entry] {
    override def compare(x: Entry, y: Entry): Int = {
      val r = x.freq - y.freq
      if (r == 0) x.order - y.order else r
    }
  }
  private[this] val pq = mutable.PriorityQueue[Entry]()(ordering)
  private[this] val freqs = mutable.Map[Int, Int]()

  private[this] var op = 0

  def push(x: Int): Unit = {
    op += 1
    val fx = freqs.getOrElse(x, 0) + 1
    freqs(x) = fx

    pq.enqueue(Entry(x, fx, op))
  }

  def pop(): Int = {
    op += 1

    val x = pq.dequeue().v
    freqs(x) = freqs(x) - 1
    if (freqs(x) == 0) freqs.remove(x)
    x
  }
}

object FreqStackApp extends LeetcodeApp {
  override def run(): Unit = {
    val fs = new FreqStack()
    fs.push(5)
    fs.push(7)
    fs.push(5)
    fs.push(7)
    fs.push(4)
    fs.push(5)
    println(fs.pop())
    println(fs.pop())
    println(fs.pop())
    println(fs.pop())
  }
}

