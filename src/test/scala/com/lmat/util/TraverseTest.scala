package com.lmat.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class TraverseTest extends AnyFunSuite with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("elements",                     "result"),
      (Seq(),                          Some(Seq())),
      (Seq(None),                      None),
      (Seq(Some(2)),                   Some(Seq(2))),
      (Seq(Some(2), Some(3), Some(1)), Some(Seq(2, 3, 1))),
      (Seq(Some(2), Some(3), None),    None),
    )

  forAll(examples) { (elements, result) =>
    test(s"Sequencing: $elements") {
      assert(Traverse.sequenceOption(elements) == result)
    }
  }
}
