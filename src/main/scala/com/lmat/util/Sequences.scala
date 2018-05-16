package com.lmat.util

object Sequences {

  /**
    * Shift right all elements in a circular sequence by the specified amount
    * Assumptions: shift is non negative
    */
  def shiftRight[T](sequence: Seq[T], shift: Int): Seq[T] =
    if(shift == 0 || sequence.isEmpty) sequence
    else {
      val (start, end) = sequence.splitAt((sequence.size - shift) % sequence.size)
      end ++ start
    }
}
