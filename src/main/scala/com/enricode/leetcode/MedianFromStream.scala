package com.enricode.leetcode

import com.enricode.util.{Heap, LeetcodeApp, MaxHeap, MinHeap}
import scala.util.Random


/**
  * https://leetcode.com/problems/find-median-from-data-stream/description/
  */
class MedianFinder() {
  private[this] val maxHeap = new MaxHeap[Int] // [0 ~ n/2]
  private[this] val minHeap = new MinHeap[Int] // (n/2 ~ n)

  private[this] def chooseHeap: Heap[Int] = {
    if (maxHeap.size == minHeap.size) maxHeap else minHeap
  }

  def addNum(num: Int): Unit = {
    val h = chooseHeap
    h.push(num)

    (maxHeap.peek(), minHeap.peek()) match {
      case (Some(x), Some(y)) =>
        if (x > y) {
          maxHeap.pop()
          minHeap.pop()

          maxHeap.push(y)
          minHeap.push(x)
        }
      case (l, r) =>
        ()
    }
  }

  def findMedian(): Double = {
    if (maxHeap.size == minHeap.size) (maxHeap.peek().toSeq ++ minHeap.peek().toSeq).sum.toDouble / 2
    else maxHeap.peek().get
  }
}

object MedianFromStream extends LeetcodeApp {
  private[this] def tests(max: Int) = {

    val expect = max.toDouble / 2 + 0.5
    (1 to 200).foreach { _ =>
      val mf = new MedianFinder()
      val randSeq = Random.shuffle((1 to max).toArray.toSeq)
      randSeq.foreach(mf.addNum)
      val got = mf.findMedian()
      if (got != expect) {
        println(s"ERROR: for $max got $got but expected $expect")
      }
    }
    println(s"done testing $max")
  }

  override def run(): Unit = {
    (16 to 80).foreach(tests)
  }
}