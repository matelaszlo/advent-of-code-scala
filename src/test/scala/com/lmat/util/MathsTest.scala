package com.lmat.util

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.util.Maths._

class MathsTest extends FunSuite with TableDrivenPropertyChecks {

  val numbers =
    Table(
      ("number", "prime", "composite"),
      (0,        false,   true),
      (1,        false,   true),
      (2,        true,    false),
      (3,        true,    false),
      (4,        false,   true),
      (5,        true,    false),
      (6,        false,   true),
      (7,        true,    false),
      (8,        false,   true),
      (9,        false,   true),
      (10,       false,   true),
      (15485863, true,    false),
      (15485861, false,   true)
    )

  forAll(numbers) { (number, prime, _) =>
    test(s"$number is prime=$prime") {
      assert(isPrime(number) == prime)
    }
  }

  forAll(numbers) { (number, _, composite) =>
    test(s"$number is composite=$composite") {
      assert(isComposite(number) == composite)
    }
  }
}
