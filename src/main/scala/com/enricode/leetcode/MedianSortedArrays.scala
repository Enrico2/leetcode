package com.enricode.leetcode

import com.enricode.util.LeetcodeApp
import scala.annotation.tailrec
import scala.util.Random

/**
  * https://leetcode.com/problems/median-of-two-sorted-arrays/description/
  */
object MedianSortedArrays extends LeetcodeApp {
  private[this] def tests(max: Int) = {
    val expect = max.toDouble / 2 + 0.5
    (1 to 15).foreach { _ =>
      val (a, b) = Random.shuffle((1 to max)).splitAt(Random.nextInt(max - 1) + 1)
      val got = findMedianSortedArrays(a.sorted.toArray, b.sorted.toArray)
      if (got != expect) {
        println(s"ERROR: For $a, $b got $got but expected $expect")
      }
    }

    println(s"done testing $max")
  }

  override def run(): Unit = {
    tests(2)
    tests(3)
    tests(4)
    tests(5)
    tests(6)
    tests(7)
  }

  def findMedianSortedArrays(a1: Array[Int], a2: Array[Int]): Double = {
    val (a, b) = if (a1.length <= a2.length) (a1, a2) else (a2, a1)
    // n >= m
    val m = a.length
    val n = b.length
    val nm = n + m


    @tailrec
    def search(imin: Int, imax: Int): Double = {
      val i = (imin + imax) / 2
      val j = ((nm + 1) / 2) - i

      if (i < imax && b(j - 1) > a(i)) {
        search(i + 1, imax)
      } else if (i > imin && a(i - 1) > b(j)) {
        search(imin, i)
      } else {
        val maxLeft = if (i == 0) b(j - 1) else if (j == 0) a(i - 1) else math.max(a(i - 1), b(j - 1))
        if (nm % 2 == 1) maxLeft else {
          val minRight = if (i == m) b(j) else if (j == n) a(i) else math.min(a(i), b(j))
          (maxLeft.toDouble + minRight.toDouble) / 2
        }
      }
    }

    search(0, m)
  }


  def findMedianSortedArrays_slow(a1: Array[Int], a2: Array[Int]): Double = {
    val n = a1.length + a2.length

    val sai = new Iterator[Int] {
      var i = 0
      var j = 0

      override def hasNext = i + j < n

      override def next() = {
        if (i == a1.length) {
          val r = a2(j)
          j += 1
          r
        } else if (j == a2.length) {
          val r = a1(i)
          i += 1
          r
        } else if (a1(i) <= a2(j)) {
          val r = a1(i)
          i += 1
          r
        } else {
          val r = a2(j)
          j += 1
          r
        }
      }
    }

    var i = 0
    val target = n / 2
    val odd = n % 2 == 1
    var cont = true
    while (sai.hasNext && cont) {
      if (i == target) {
        cont = false
      } else {
        i += 1
        sai.next()
      }
    }

    if (odd) {
      sai.next()
    } else {
      (sai.next() + sai.next()) / 2
    }
  }
}
