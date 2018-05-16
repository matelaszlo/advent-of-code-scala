package com.lmat.util

import scala.io.Source

object Files {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[String] = {
    val resourcePath = getClass.getClassLoader.getResource(resource).getPath
    Source.fromFile(resourcePath).getLines().toSeq
  }
}
