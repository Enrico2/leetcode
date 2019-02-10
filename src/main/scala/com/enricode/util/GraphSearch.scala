package com.enricode.util

import scala.collection.mutable

object GraphSearch {
  def bfs[V](
    v: V,
    nexts: V => Set[(V, V)],
    visit: V => Unit,
    alreadyVisited: V => Unit
  ): Unit = {
    val queue = mutable.Queue[V]()
    search[V](
      v,
      nexts,
      visit,
      alreadyVisited,
      n => queue.enqueue(n),
      queue.dequeue,
      queue.nonEmpty _
    )
  }

  def dfs[V](
    v: V,
    nexts: V => Set[(V, V)],
    visit: V => Unit,
    alreadyVisited: V => Unit
  ): Unit = {
    val stack = mutable.Stack[V]()
    search[V](
      v,
      nexts,
      visit,
      alreadyVisited,
      n => stack.push(n),
      stack.pop,
      stack.nonEmpty _
    )
  }

  private[this] def search[V](
    v: V,
    nexts: V => Set[(V, V)],
    visit: V => Unit,
    alreadyVisited: V => Unit,
    add: V => Unit,
    remove: () => V,
    nonEmpty: () => Boolean
  ): Unit = {
    val visited = mutable.Set[V]()
    add(v)

    while (nonEmpty()) {
      val n = remove()

      if (!visited(n)) {
        visit(n)
        visited.add(n)

        nexts(v).foreach { case (v1, v2) =>
          if (n == v1) add(v2) else add(v1)
        }
      } else {
        alreadyVisited(n)
      }
    }
  }
}
