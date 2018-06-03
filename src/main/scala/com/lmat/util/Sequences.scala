package com.lmat.util

object Sequences {

  /**
    * Shift left all elements in a circular sequence by the specified amount
    */
  def shiftLeft[T](sequence: Seq[T], shift: Int): Seq[T] =
    if (shift < 0) shiftRight(sequence, Math.abs(shift))
    else shiftRight(sequence, sequence.size - shift)

  /**
    * Shift right all elements in a circular sequence by the specified amount
    */
  def shiftRight[T](sequence: Seq[T], shift: Int): Seq[T] =
    if(shift == 0 || sequence.isEmpty) sequence
    else {
      val (start, end) = sequence.splitAt((sequence.size - shift) % sequence.size)
      end ++ start
    }
}
