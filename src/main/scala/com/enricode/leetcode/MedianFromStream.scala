package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable
import scala.util.Random


/**
  * https://leetcode.com/problems/find-median-from-data-stream/description/
  */
class MedianFinder() {
  private[this] val maxHeap = new mutable.PriorityQueue[Int]()
  private[this] val minHeap = new mutable.PriorityQueue[Int]().reverse

  private[this] def chooseHeap: mutable.PriorityQueue[Int] = {
    if (maxHeap.size == minHeap.size) maxHeap else minHeap
  }

  def addNum(num: Int): Unit = {
    val h = chooseHeap
    h.enqueue(num)

    (maxHeap.headOption, minHeap.headOption) match {
      case (Some(x), Some(y)) =>
        if (x > y) {
          maxHeap.dequeue()
          minHeap.dequeue()

          maxHeap.enqueue(y)
          minHeap.enqueue(x)
        }
      case (l, r) =>
        ()
    }
  }

  def findMedian(): Double = {
    if (maxHeap.size == minHeap.size) (maxHeap.headOption.toSeq ++ minHeap.headOption.toSeq).sum.toDouble / 2
    else maxHeap.head
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