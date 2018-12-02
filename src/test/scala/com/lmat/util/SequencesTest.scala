package com.lmat.util

import com.lmat.util.Sequences._
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

  val cycles =
    Table(
      ("source",                 "take", "result"),
      (Seq(),                    5,      Stream()),
      (Seq(1),                   5,      Stream(1, 1, 1, 1, 1)),
      (Seq(1),                   10,     Stream(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
      (Seq(1, -1),               5,      Stream(1, -1, 1, -1, 1)),
      (Seq(1, -1),               10,     Stream(1, -1, 1, -1, 1, -1, 1, -1, 1, -1)),
      (Seq(1, -1, 2, -2, 3, -3), 5,      Stream(1, -1, 2, -2, 3)),
      (Seq(1, -1, 2, -2, 3, -3), 10,     Stream(1, -1, 2, -2, 3, -3, 1, -1, 2, -2)),
    )

  forAll(cycles) { (source, take, result) =>
    test(s"Cycle $source and take $take") {
      assert(cycle(source).take(take) == result)
    }
  }

  val monoids = Map[String, (Int, (Int, Int) => Int)](
    "plus" -> (0, _ + _),
    "times" -> (1, _ * _)
  )

  val cumulatives =
    Table(
      ("source",              "monoid", "result"),
      (Stream(),              "plus",   Stream(0)),
      (Stream(),              "times",  Stream(1)),
      (Stream(1),             "plus",   Stream(0, 1)),
      (Stream(1),             "times",  Stream(1, 1)),
      (Stream(1, 2, 3, 4, 5), "plus",   Stream(0, 1, 3, 6, 10, 15)),
      (Stream(1, 2, 3, 4, 5), "times",  Stream(1, 1, 2, 6, 24, 120)),
    )

  forAll(cumulatives) { (source, monoid, result) =>
    test(s"Cumulative ${source.take(10).toList} with $monoid") {
      assert(cumulative(monoids(monoid)._1, monoids(monoid)._2)(source) == result)
    }
  }

  val duplicates =
    Table(
      ("source",                        "result"),
      (Stream(),                        None),
      (Stream(1, 2, 3, 4, 5),           None),
      (Stream("1", "2", "3", "4"),      None),
      (Stream(1, 2, 3, 4, 5, 3, 1, 2),  Some(3)),
    )

  forAll(duplicates) { (source, result) =>
    test(s"FindFirstDuplicate ${source.take(10).toList}") {
      assert(findFirstDuplicate(source) == result)
    }
  }

  val elements =
    Table(
      ("source",                                  "result"),
      (Stream(),                                  Map()),
      (Seq(1, 2, 3, 4, 5),                        Map(1   -> 1,  2  -> 1,  3  -> 1,  4  -> 1,  5  -> 1)),
      (Stream("1", "2", "3", "4", "3", "3", "1"), Map("1" -> 2, "2" -> 1, "3" -> 3, "4" -> 1)),
      (Vector(1, 2, 3, 4, 5, 3, 1, 2),            Map(1   -> 2,  2  -> 2,  3  -> 2,  4  -> 1,  5  -> 1)),
      ("1122334455".toCharArray.toSeq,            Map('1' -> 2, '2' -> 2, '3' -> 2, '4' -> 2, '5' -> 2)),
    )

  forAll(elements) { (source, result) =>
    test(s"Count all elements ${source}") {
      assert(countElements(source) == result)
    }
  }
}
