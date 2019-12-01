package com.lmat.util

import com.lmat.util.Strings._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class StringsTest extends AnyFunSuite with TableDrivenPropertyChecks {

  val stringsForPadding =
    Table(
      ("Source",      "Pad", "Length", "Result"),
      ("",            '0',   0,         ""),
      ("",            '0',   1,         "0"),
      ("0",           '0',   0,         "0"),
      ("0",           '0',   1,         "0"),
      ("0",           '0',   -1,        "0"),
      ("123",         '0',   1,         "123"),
      ("123",         '0',   4,         "0123"),
      ("123",         '0',   10,        "0000000123"),
    )

  forAll(stringsForPadding) { (source, pad, length, result) =>
    test(s"Left Pad $source with $pad to $length") {
      assert(leftPad(source)(pad, length) == result)
    }
  }

  val stringsForIndices =
    Table(
      ("Source",   "Query", "Indices"),
      ("",         "",      Seq()),
      ("",         "aa",    Seq()),
      ("aa",       "",      Seq((0,0), (1,1))),
      ("aaa",      "aaa",   Seq((0, 3))),
      ("aaa",      "aa",    Seq((0, 2), (1, 3))),
      ("aaa",      "a",     Seq((0, 1), (1, 2), (2, 3))),
      ("abbaabba", "aa",    Seq((3, 5))),
    )

  forAll(stringsForIndices) { (source, query, indices) =>
    test(s"IndicesOf $query in $source") {
      assert(indicesOf(source, query) == indices)
    }
  }
}
