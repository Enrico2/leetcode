package com.enricode.util

import scala.util.Random

class HeapTest extends org.scalatest.FunSuite {
  test("MinHeap") {
    val h = new MinHeap[Int]

    Random.shuffle((1 to 2000).toArray.toSeq).foreach(h.push)
    assert(h.peek() == Some(1))
  }

  test("MaxHeap") {
    val h = new MinHeap[Int]

    Random.shuffle((1 to 2000).toArray.toSeq).foreach(h.push)
    assert(h.peek() == Some(2000))
  }
}
