package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

object LFUCache extends LeetcodeApp {
  override def run(): Unit = {
    val cache = new LFUCache(10)
    cache.put(10, 13)
    cache.put(3, 17)
    cache.put(6, 11)
    cache.put(10, 5)
    cache.put(9, 10)
    println(cache.get(13))
    cache.put(2, 19)
    println(cache.get(2))
    println(cache.get(3))
    cache.put(5, 25)
    println(cache.get(8))
    cache.put(9, 22)
    cache.put(5, 5)
    cache.put(1, 30)
    println(cache.get(11))
    cache.put(9, 12)
    println(cache.get(7))
    println(cache.get(5))
    println(cache.get(8))
    println(cache.get(9))
    cache.put(4, 30)
    cache.put(9, 3)
    println(cache.get(9))
    println(cache.get(10))
    println(cache.get(10))
    cache.put(6, 14)
    cache.put(3, 1)
    println(cache.get(3))
    cache.put(10, 11)
    println(cache.get(8))
    cache.put(2, 14)
    println(cache.get(1))
    println(cache.get(5))
    println(cache.get(4))
    cache.put(11, 4)
    cache.put(12, 24)
    cache.put(5, 18)
    println(cache.get(13))
    cache.put(7, 23)
    println(cache.get(8))
    println(cache.get(12))
    cache.put(3, 27)
    cache.put(2, 12)
    println(cache.get(5))
    cache.put(2, 9)
    cache.put(13, 4)
    cache.put(8, 18)
    cache.put(1, 7)
    println(cache.get(6))
    cache.put(9, 29)
    cache.put(8, 21)
    println(cache.get(5))
    cache.put(6, 30)
    cache.put(1, 12)
    println(cache.get(10))
    cache.put(4, 15)
    cache.put(7, 22)
    cache.put(11, 26)
    cache.put(8, 17)
    cache.put(9, 29)
    println(cache.get(5))
    cache.put(3, 4)
    cache.put(11, 30)
    println(cache.get(12))
    cache.put(4, 29)
    println(cache.get(3))
    println(cache.get(9))
    println(cache.get(6))
    cache.put(3, 4)
    println(cache.get(1))
    println(cache.get(10))
    cache.put(3, 29)
    cache.put(10, 28)
    cache.put(1, 20)
    cache.put(11, 13)
    println(cache.get(3))
    cache.put(3, 12)
    cache.put(3, 8)
    cache.put(10, 9)
    cache.put(3, 26)
    println(cache.get(8))
    println(cache.get(7))
    println(cache.get(5))
    cache.put(13, 17)
    cache.put(2, 27)
    cache.put(11, 15)
    println(cache.get(12))
    cache.put(9, 19)
    cache.put(2, 15)
    cache.put(3, 16)
    println(cache.get(1))
    cache.put(12, 17)
    cache.put(9, 1)
    cache.put(6, 19)
    println(cache.get(4))
    println(cache.get(5))
    println(cache.get(5))
    cache.put(8, 1)
    cache.put(11, 7)
    cache.put(5, 2)
    cache.put(9, 28)
    println(cache.get(1))
    cache.put(2, 2)
    cache.put(7, 4)
    cache.put(4, 22)
    cache.put(7, 24)
    cache.put(9, 26)
    cache.put(13, 28)
    cache.put(11, 26)
  }
}

class LFUCache(capacity: Int) {
  var size = 0
  val keyToCount = mutable.HashMap[Int, Count]()
  val keyToValue = mutable.HashMap[Int, Int]()
  var lowFreq: Option[Count] = None

  class Count(val freq: Int) {
    val keys = mutable.LinkedHashSet[Int]()
    var next: Option[Count] = None
    var prev: Option[Count] = None
  }

  private[this] def addKeyToNext(count: Count, key: Int): Unit = {
    count.next match {
      case Some(next) if (next.freq == count.freq+1) =>
        next.keys.add(key)
        keyToCount += ((key, next))

      case nextNext =>
        val next = new Count(count.freq+1)
        next.keys.add(key)
        count.next = Some(next)
        next.prev = Some(count)
        keyToCount += ((key, next))

        nextNext.foreach(_.prev = Some(next))
    }
  }

  private[this] def removeNode(count: Count): Unit = {
    if (lowFreq.contains(count)) {
      lowFreq = count.next
      count.next.foreach(_.prev = None)
    } else {
      count.prev.foreach(_.next = count.next)
      count.next.foreach(_.prev = count.prev)
    }
  }

  private[this] def incrFrequency(key: Int): Unit = {
    keyToCount.get(key) match {
      case Some(count) =>
        count.keys.remove(key)
        keyToCount.remove(key)
        addKeyToNext(count, key)
        if (count.keys.isEmpty) removeNode(count)

      case None =>
        lowFreq match {
          case Some(count) if (count.freq == 1) =>
            count.keys.add(key)
            keyToCount += ((key, count))
          case lf =>
            val count = new Count(1)
            keyToCount += ((key, count))
            count.keys.add(key)
            count.next = lf
            lf.foreach(_.prev = Some(count))
            lowFreq = Some(count)
        }

    }
  }

  private[this] def removeLFU(): Unit = {
    lowFreq match {
      case Some(count) =>
        val k = count.keys.head
        keyToValue.remove(k)
        count.keys.remove(k)
        keyToCount.remove(k)

        if (count.keys.isEmpty) removeNode(count)

      case None => ()
    }
  }

  def get(key: Int): Int = {
    if (capacity == 0) return -1

    keyToValue.get(key).map { value =>
      incrFrequency(key)
      value
    }.getOrElse(-1)
  }

  def put(key: Int, value: Int): Unit = {
    if (capacity == 0) return

    if (keyToValue.contains(key)) {
      keyToValue += ((key, value))
      incrFrequency(key)
    } else {
      if (size == capacity) {
        removeLFU()
        size -= 1
        put(key, value)
      } else {
        keyToValue += ((key, value))
        incrFrequency(key)
        size += 1
      }
    }
  }
}