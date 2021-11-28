package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day02Definitions._
import com.lmat.util.Files.readResource

object Day02Definitions {
  case class Policy(char: Char, min: Int, max: Int)
}

object Day02 extends SimpleCommonPuzzle[List[(Policy, String)], Int, Int] {
  override def parse(resource: String): List[(Policy, String)] =
    readResource(resource).flatMap(parsePolicyRow).toList

  def parsePolicyRow(row: String): Option[(Policy, String)] = {
    val pattern = s"(\\d+)-(\\d+) (.): (.*)".r

    row match {
      case pattern(minRaw, maxRaw, charRaw, password) => for {
        char <- charRaw.toCharArray.headOption
        min <- minRaw.toIntOption
        max <- maxRaw.toIntOption
      } yield (Policy(char, min, max), password)
      case _ => None
    }
  }

  override def part1(passwords: List[(Policy, String)]): Int =
    passwords.count { case (policy, pass) => isValid(policy)(pass) }

  override def part2(passwords: List[(Policy, String)]): Int =
    passwords.count { case (policy, pass) => isValid2(policy)(pass) }

  def isValid(policy: Policy)(password: String): Boolean = {
    val givenCharCount = password.count(_ == policy.char)
    policy.min <= givenCharCount && givenCharCount <= policy.max
  }

  def isValid2(policy: Policy)(password: String): Boolean = {
    val relevantChars = List(policy.min, policy.max).map(_ - 1).flatMap(password.lift)
    relevantChars.contains(policy.char) && relevantChars.toSet.size != 1
  }
}
