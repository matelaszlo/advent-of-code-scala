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

  /**
    * Swap the elements in pos1 and pos2 in a sequence
    * @throws IndexOutOfBoundsException if `pos1` or `pos2` does not satisfy `0 <= index < length`.
    */
  def swap[A](sequence: Seq[A])(pos1: Int, pos2: Int): Seq[A] =
    sequence.updated(pos1, sequence(pos2)).updated(pos2, sequence(pos1))
}
