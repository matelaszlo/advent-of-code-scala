package com.lmat.util

import com.lmat.util.Sequences.{shiftLeft, shiftRight, swap}
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

  val swaps =
    Table(
      ("source",        "pos1", "pos2",   "result"),
      (Seq(1),          0,      0,        Seq(1)),
      (Seq(1, 2),       0,      1,        Seq(2, 1)),
      (Seq(1, 2, 3, 4), 0,      1,        Seq(2, 1, 3, 4)),
      (Seq(1, 2, 3, 4), 3,      0,        Seq(4, 2, 3, 1)),
      (Seq(1, 2, 3, 4), 1,      2,        Seq(1, 3, 2, 4)),
    )

  forAll(swaps) { (source, pos1, pos2, result) =>
    test(s"Swap $pos1 with $pos2 in $source") {
      assert(swap(source)(pos1, pos2) == result)
    }
  }

  val illegalSwaps =
    Table(
      ("source",        "pos1", "pos2"),
      (Seq(),           0,      0),
      (Seq(1),          0,      1),
      (Seq(1, 2, 3, 4), 0,      4),
      (Seq(1, 2, 3, 4), 3,      -1),
      (Seq(1, 2, 3, 4), 6,      7),
    )

  forAll(illegalSwaps) { (source, pos1, pos2) =>
    test(s"Illegal Swap $pos1 with $pos2 in $source") {
      assertThrows[IndexOutOfBoundsException] {
        swap(source)(pos1, pos2)
      }
    }
  }
}
