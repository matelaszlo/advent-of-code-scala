package com.lmat.util

import com.lmat.util.Sequences.{shiftLeft, shiftRight}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class SequencesTest extends FunSuite with TableDrivenPropertyChecks {

  val sequences =
    Table(
      ("source",      "shift", "left",        "right"),
      (Seq(),         0,        Seq(),        Seq()),
      (Seq(),         1,        Seq(),        Seq()),
      (Seq(1),        0,        Seq(1),       Seq(1)),
      (Seq(1),        1,        Seq(1),       Seq(1)),
      (Seq(1, 2),     0,        Seq(1, 2),    Seq(1, 2)),
      (Seq(1, 2),     1,        Seq(2, 1),    Seq(2, 1)),
      (Seq(1, 2),     2,        Seq(1, 2),    Seq(1, 2)),
      (Seq(1, 2, 3),  0,        Seq(1, 2, 3), Seq(1, 2, 3)),
      (Seq(1, 2, 3),  1,        Seq(2, 3, 1), Seq(3, 1, 2)),
      (Seq(1, 2, 3),  2,        Seq(3, 1, 2), Seq(2, 3, 1)),
      (Seq(1, 2, 3),  -1,       Seq(3, 1, 2), Seq(2, 3, 1)),
      (Seq(1, 2, 3),  -2,       Seq(2, 3, 1), Seq(3, 1, 2)),
      (Seq(1, 2, 3),  30,       Seq(1, 2, 3), Seq(1, 2, 3)),
      (Seq(1, 2, 3),  -30,      Seq(1, 2, 3), Seq(1, 2, 3)),
    )

  forAll(sequences) { (source, shift,  result, _) =>
    test(s"Shift $source by $shift to the left") {
      assert(shiftLeft(source, shift) == result)
    }
  }

  forAll(sequences) { (source, shift,  _, result) =>
    test(s"Shift $source by $shift to the right") {
      assert(shiftRight(source, shift) == result)
    }
  }

}
