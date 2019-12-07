package com.lmat.util

object Logic {
  def and[A](fs: (A => Boolean)*): A => Boolean =
    fs.reduce[A => Boolean]{ case (f1, f2) => n => f1(n) && f2(n) }

  def or[A](fs: (A => Boolean)*): A => Boolean =
    fs.reduce[A => Boolean]{ case (f1, f2) => n => f1(n) || f2(n) }
}
