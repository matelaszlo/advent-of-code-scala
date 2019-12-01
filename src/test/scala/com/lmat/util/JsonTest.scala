package com.lmat.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonTest extends AnyFunSuite with TableDrivenPropertyChecks {

  val examples =
    Table(
      ("raw",          "json"),
      ("""123""",      JsonIntValue(123)),
      ("""-123""",     JsonIntValue(-123)),
      (""""magic"""",  JsonStringValue("magic")),
      ("""[]""",       JsonArray(Seq())),
      ("""{}""",       JsonObject(Map())),

      ("""[1,2,3]""",             JsonArray(Seq(JsonIntValue(1), JsonIntValue(2), JsonIntValue(3)))),
      ("""[1,"two",3]""",         JsonArray(Seq(JsonIntValue(1), JsonStringValue("two"), JsonIntValue(3)))),
      ("""[[[3]]]""",             JsonArray(Seq(JsonArray(Seq(JsonArray(Seq(JsonIntValue(3)))))))),
      ("""[[[3],"five"],[4]]""",  JsonArray(Seq(JsonArray(Seq(JsonArray(Seq(JsonIntValue(3))), JsonStringValue("five"))), JsonArray(Seq(JsonIntValue(4)))))),

      ("""{"a":2,"b":4}""",        JsonObject(Map("a" -> JsonIntValue(2), "b" -> JsonIntValue(4)))),
      ("""{"a":{"b":4},"c":-1}""", JsonObject(Map("a" -> JsonObject(Map("b" -> JsonIntValue(4))), "c" -> JsonIntValue(-1)))),
      ("""{"a":[-1,1]}""",         JsonObject(Map("a" -> JsonArray(Seq(JsonIntValue(-1), JsonIntValue(1)))))),
      ("""[-1,{"a":1}]""",         JsonArray(Seq(JsonIntValue(-1), JsonObject(Map("a" -> JsonIntValue(1)))))),
    )

  forAll(examples) { (raw, json) =>
    test(s"Parsing: $raw") {
      assert(Json.parse(raw).get == json)
    }
  }
}
