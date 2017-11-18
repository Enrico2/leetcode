package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
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

  case class QueueNode(var n: ListNode) {
    def setToNext = {
      if (n != null) n = n.next
    }

    def value = if (n != null) n.x else Int.MaxValue
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    if (lists.isEmpty) return new ListNode()
    else if (lists.forall(_ == null)) return null

    implicit val ordering = new Ordering[QueueNode] {
      override def compare(x: QueueNode, y: QueueNode): Int = x.value - y.value
    }

    val q = new mutable.PriorityQueue[QueueNode]().reverse
    val nodes = lists.flatMap { ln =>
      if (ln != null) Some(QueueNode(ln)) else None
    }
    q.enqueue(nodes:_*)

    var head: ListNode = null
    var curr = head
    while (q.nonEmpty) {
      val qn = q.dequeue()

      if (head == null) {
        head = qn.n
        curr = head
      } else {
        curr.next = qn.n
        curr = qn.n
      }
      qn.setToNext

      if (qn.n != null) q.enqueue(qn)
    }

    head
  }
}
