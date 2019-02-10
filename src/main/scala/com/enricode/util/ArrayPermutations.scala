package com.enricode.util

object ArrayPermutations {


  /**
    * Let n = state.length
    * state is an array of unique integers from 1 to n-1.
    * This function returns a new state which is the value of the next smallest permutation bigger than state
    * if state represented an n-digit number.
    *
    * @param state
    * @return
    */
  private[this] def nextState(state: List[Int]): Option[List[Int]] = {
    if (state.sorted.reverse == state)
      None
    else {
      nextState(state.tail) match {
        case Some(next) =>
          Some(state.head :: next)
        case None =>
          val head = state.head

          val next = state.tail.foldLeft(Int.MaxValue) { (curr, i) =>
            if (i < curr && i > head) i else curr
          }

          val res = Some(next :: (head :: (state.tail.filter(_ != next))).sorted)
          res
      }
    }
  }

  def apply[T](a: Seq[T]): Stream[Seq[T]] = {
    val n = a.length
    val origin = (1 to n).toList

    val fact = origin.foldLeft(1L) { (x, y) => x * y.toLong }
    val limit = if (fact > Int.MaxValue) Int.MaxValue else fact.toInt

    val in = Stream.iterate((a, origin), limit) { case (_, state) =>
      nextState(state) match {
        case Some(newState) =>
          val newA = newState.map { i => a(i - 1) }
          (newA, newState)
        case None =>
          (a, origin)
      }
    }

    in.map { case (a, _) => a }
  }
}
