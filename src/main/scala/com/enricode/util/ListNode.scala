package com.enricode.util

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x

  override def toString = s"$x -> ${if (next != null) next.toString else "null"}"
}

object ListNode {

  case class NodeBuilder(last: ListNode, head: ListNode) {
    def -->(x: Int): NodeBuilder = {
      val newLast = new ListNode(x)
      last.next = newLast
      copy(last = newLast)
    }
  }

  def -->(x: Int): NodeBuilder = {
    val node = new ListNode(x)
    NodeBuilder(node, node)
  }
}