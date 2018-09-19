package com.lmat.util

object Traverse {
  def sequenceOption[T](elements: Seq[Option[T]]): Option[Seq[T]] =
    if (elements.exists(_.isEmpty)) None
    else Some(elements.map(_.get))
}
