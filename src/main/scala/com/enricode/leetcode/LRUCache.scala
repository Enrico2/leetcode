package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.collection.mutable

/**
  * https://leetcode.com/problems/lru-cache/description/
  * @param capacity
  */
class LRUCache(capacity: Int) {
  case class Node(var prev: Option[Node], var next: Option[Node], k: Int, var v: Int)

  private[this] var size = 0
  private[this] var head: Option[Node] = None
  private[this] var last: Option[Node] = None

  val map = mutable.HashMap[Int, Node]()

  private[this] def removeLRU(): Unit = {
    last.foreach { l =>
      map.remove(l.k)
      l.prev.foreach { p =>
        p.next = None
      }
      last = l.prev
    }
  }

  private[this] def updateMRU(n: Node): Unit = {
    if (!head.contains(n)) {
      n.prev.foreach { prev =>
        prev.next = n.next
      }
      n.next.foreach { next =>
        next.prev = n.prev
      }
      if (last.contains(n)) {
        last = n.prev
      }
      n.next = head
      n.prev = None

      head.foreach { h => h.prev = Some(n) }
      head = Some(n)
    }
  }

  def get(key: Int): Int = {
    map.get(key).map { node =>
      updateMRU(node)
      node.v
    }.getOrElse(-1)
  }

  def put(key: Int, value: Int) {
    if (map.contains(key)) {
      val n = map(key)
      n.v = value
      updateMRU(n)
    } else {
      val node = Node(None, head, key, value)

      if (size == capacity) {
        removeLRU()
      } else {
        size += 1
      }

      if (size == 1) {
        last = Some(node)
      }

      map(key) = node
      head.foreach(_.prev = Some(node))
      head = Some(node)
    }
  }
}

object Solution extends LeetcodeApp {

  override def run(): Unit = {
    val cache = new LRUCache(2)
    cache.put(1, 1)
    cache.put(2, 2)
    println(cache.get(1))  // returns 1
    cache.put(3, 3)        // evicts key 2
    println(cache.get(2))  // returns -1 (not found)
    cache.put(4, 4)        // evicts key 1
    println(cache.get(1))  // returns -1 (not found)
    println(cache.get(3))  // returns 3
    println(cache.get(4))  // returns 4
  }
}
/**
  * Your LRUCache object will be instantiated and called as such:
  * var obj = new LRUCache(capacity)
  * var param_1 = obj.get(key)
  * obj.put(key,value)
  */