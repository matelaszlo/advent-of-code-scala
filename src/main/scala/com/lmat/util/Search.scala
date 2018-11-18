package com.lmat.util

object Search {

  /**
    * Using the lazy nature of streams
    * We can safely iterate over all possible states (given a starting state)
    * - We can find all possible solution for puzzles (Given that the state space is small enough)
    * - We can find the shortest solution for puzzles (Given that our children function returns all child states sorted for our length criteria)
    */
  def streamSearch[A](initial: Stream[A], children: A => Stream[A]): Stream[A] = {
    val more = initial.flatMap(children)
    if (more.isEmpty) initial
    else initial #::: streamSearch(more, children)
  }
}
