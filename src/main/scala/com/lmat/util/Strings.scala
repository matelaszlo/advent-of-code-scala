package com.lmat.util

object Strings {
  /**
    * Left pad a string to a length with the specified character
    */
  def leftPad(source: String)(pad: Char, length: Int): String =
    (pad.toString * (length - source.length)) + source
}
