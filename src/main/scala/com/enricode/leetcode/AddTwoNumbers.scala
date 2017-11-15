package com.enricode.leetcode

import com.enricode.util.LeetcodeApp

/**
  * https://leetcode.com/problems/add-two-numbers/description/
  */
object AddTwoNumbers extends LeetcodeApp {
  override def run(): Unit = {
    val l11 = new ListNode(2)
    val l12 = new ListNode(4)
    val l13 = new ListNode(3)

    val l21 = new ListNode(0)
    val l22 = new ListNode(6)
    val l23 = new ListNode(4)
    val l24 = new ListNode(4)

    l11.next = l12
    l12.next = l13

    l21.next = l22
    l22.next = l23
    l23.next = l24

    println(addTwoNumbers(l11, l21))

  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var n1 = l1
    var n2 = l2

    var i = 0

    var head: ListNode = null
    var prev = head
    var carry = 0
    while (n1 != null || n2 != null) {
      val v1 = if (n1 == null) 0 else n1._x
      val v2 = if (n2 == null) 0 else n2._x

      val s = v1+v2+carry
      carry = s / 10

      val nextNode = new ListNode(s % 10)
      if (head == null) {
        head = nextNode
        prev = head
      } else {
        prev.next = nextNode
        prev = nextNode
      }

      i += 1
      n1 = if (n1 == null) null else n1.next
      n2 = if (n2 == null) null else n2.next
    }

    if (carry > 0) {
      val n = new ListNode(carry)
      prev.next = n
    }

    head
  }

}


class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x

  override def toString = s"$x -> ${if (next != null) next.toString else "null" }"
}