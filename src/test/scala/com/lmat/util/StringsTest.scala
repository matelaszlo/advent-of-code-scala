package com.lmat.util

import com.lmat.util.Strings.leftPad
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class StringsTest extends FunSuite with TableDrivenPropertyChecks {

  val strings =
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

  forAll(strings) { (source, pad, length, result) =>
    test(s"Left Pad $source with $pad to $length") {
      assert(leftPad(source)(pad, length) == result)
    }
  }
}
