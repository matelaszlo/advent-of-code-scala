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

  val divisorsTable =
    Table(
      ("number", "divisors"),
      (1,        Set(1)),
      (2,        Set(1, 2)),
      (4,        Set(1, 2, 4)),
      (5,        Set(1, 5)),
      (6,        Set(1, 2, 3, 6)),
      (7,        Set(1, 7)),
      (8,        Set(1, 2, 4, 8)),
      (9,        Set(1, 3, 9)),
      (10,       Set(1, 2, 5, 10)),
      (15485863, Set(1, 15485863)),
      (15485861, Set(1, 17, 503, 1811, 8551, 30787, 910933, 15485861)),
    )

  forAll(divisorsTable) { (number, divisorsResult) =>
    test(s"Divisors of $number") {
      assert(divisors(number) == divisorsResult)
    }
  }
}
