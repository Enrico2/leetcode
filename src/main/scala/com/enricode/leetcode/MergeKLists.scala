package com.enricode.leetcode

import com.enricode.util.{LeetcodeApp, ListNode}
import scala.collection.mutable

/**
  * https://leetcode.com/problems/merge-k-sorted-lists/description/
  */
object MergeKLists extends LeetcodeApp {

  override def run(): Unit = {
    val lists = Array(
      (ListNode --> 4 --> 6 --> 19 --> 200 --> 201).head,
      (ListNode --> 3 --> 8 --> 9 --> 202 --> 208).head,
      (ListNode --> 17 --> 18 --> 19 --> 20 --> 21).head,
      (ListNode --> -1).head,
      (ListNode --> -2 --> 0 --> 5 --> 209).head
    )

    println(mergeKLists(lists))
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    val ordering = new Ordering[ListNode] {
      override def compare(x: ListNode, y: ListNode): Int = x.x - y.x
    }

    val pq = mutable.PriorityQueue[ListNode](lists: _*)(ordering).reverse

    val head = pq.dequeue()
    var curr = head

    if (curr.next != null) pq.enqueue(curr.next)

    while (pq.nonEmpty) {
      val next = pq.dequeue()
      if (next.next != null) pq.enqueue(next.next)
      curr.next = next
      curr = next
    }

    head
  }
}
