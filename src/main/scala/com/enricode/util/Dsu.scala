package com.enricode.util

import scala.collection.mutable

class Dsu[T] {

  case class Parent(followers: Int, t: T)

  private[this] val parent = mutable.Map[T, Parent]()

  def find(t: T): T = {
    var x = t
    while (x != parent.getOrElseUpdate(x, Parent(0, x)).t) {
      x = parent(x).t
    }
    x
  }

  def union(t1: T, t2: T): Boolean = {
    val p1 = parent(find(t1))
    val p2 = parent(find(t2))

    if (p1 == p2) {
      false
    } else {
      if (p1.followers >= p2.followers) {
        parent(p2.t) = p1.copy(followers = p1.followers + p2.followers)
      } else {
        parent(p1.t) = p2.copy(followers = p1.followers + p2.followers)
      }
      true
    }
  }
}


