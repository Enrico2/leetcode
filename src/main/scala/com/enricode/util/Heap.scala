package com.enricode.util

import scala.annotation.tailrec
import scala.collection.mutable

class MaxHeap[T <% Ordered[T]] extends Heap[T](_ > _)

class MinHeap[T <% Ordered[T]] extends Heap[T](_ <= _)

abstract class Heap[T <% Ordered[T]](compare: (T, T) => Boolean) {
  private[this] val a = mutable.ArrayBuffer[T]()

  private[this] def left(n: Int) = 2 * n + 1

  private[this] def right(n: Int) = 2 * n + 2

  private[this] def parent(n: Int) = if (n % 2 == 0) n / 2 - 1 else n / 2

  def size = a.size

  def length = a.length

  def peek(): Option[T] = a.headOption

  def push(t: T): Unit = {
    a.append(t)
    bubbleUp(a.length - 1)
  }

  def pop(): Option[T] = {
    if (a.nonEmpty) {
      val res = a(0)
      a(0) = a(length - 1)
      a.remove(length - 1)
      bubbleDown(0)
      Some(res)
    } else None
  }

  private[this] def swap(i: Int, j: Int) = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  private[this] def checkNode(i: Int): Boolean = {
    val l = left(i)
    val r = right(i)

    val lCheck = if (l < a.length) compare(a(i), a(l)) else true
    val rCheck = if (r < a.length) compare(a(i), a(r)) else true
    lCheck && rCheck
  }

  @tailrec
  private[this] def bubbleUp(i: Int): Unit = {
    if (i > 0) {
      val pi = parent(i)
      if (!checkNode(pi)) {
        swap(pi, i)
        bubbleUp(pi)
      }
    }
  }

  private[this] def choose(l: Int, r: Int): Int = {
    if (l < a.length) {
      if (r < a.length) {
        if (compare(a(l), a(r))) l else r
      } else l
    } else {
      -1
    }
  }

  private[this] def bubbleDown(i: Int): Unit = {
    if (i < a.length) {
      if (!checkNode(i)) {
        val l = left(i)
        val r = right(i)

        val s = choose(l, r)
        swap(i, s)
        bubbleDown(s)
      }
    }
  }
}