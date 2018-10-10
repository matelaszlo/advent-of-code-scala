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

  /**
    * Cycle a sequence as an infinite Stream
    * [0, 1] => [0, 1, 0, 1, ... [
    */
  def cycle[A](sequence: Seq[A]): Stream[A] =
    if (sequence.isEmpty) Stream.empty[A]
    else Stream.continually(sequence).flatten

  /**
    * Transform a stream into its cumulative equivalent
    * Using empty and combine
    *
    * Using:  0, _ + _
    * [1, 2, 3, 4, 5, .. [ => [0, 1, 3, 6, 10, 15 .. [
    *
    * Using:  1, _ * _
    * [1, 2, 3, 4, 5, .. [ => [1, 1, 2, 6, 24, 100 .. [
    */
  def cumulative[A](empty: A, combine: (A, A) => A)(stream: Stream[A]): Stream[A] =
    stream.scan(empty)(combine)
}
