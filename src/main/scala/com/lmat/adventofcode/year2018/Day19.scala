package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day16Definitions.Instruction
import com.lmat.adventofcode.year2018.Day19Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.{Maths, Traverse}

import scala.util.Try

object Day19Definitions {
  case class Program(pointer: Int, instructions: List[Instruction])
}

object Day19 extends SimpleCommonPuzzle[Program, Int, Int] {
  override def parse(resource: String): Program = parseProgram(readResource(resource)).get

  def parseProgram(rows: Seq[String]): Option[Program] = {
    val rawPointer +: rawInstructions = rows
    for {
      pointer <- parsePointer(rawPointer)
      instructions <- Traverse.sequenceOption[Instruction](rawInstructions.map(parseInstruction))
    } yield Program(pointer, instructions.toList)
  }

  def parsePointer(row: String): Option[Int] = {
    val pattern = s"#ip (.*)".r
    row match {
      case pattern(rawPointer) => Try(rawPointer.toInt).toOption
      case _ => None
    }
  }

  def parseInstruction(row: String): Option[Instruction] = {
    val pattern = s"(.*) (.*) (.*) (.*)".r
    row match {
      case pattern(name, rawA, rawB, rawC) =>
        for {
          a <- Try(rawA.toInt).toOption
          b <- Try(rawB.toInt).toOption
          c <- Try(rawC.toInt).toOption
        } yield Instruction(name, a, b, c)
      case _ => None
    }
  }

  // We can reuse Day 16's instruction definition and application
  def run(program: Program, startingState: Map[Int, Int]): LazyList[Map[Int, Int]] = {
    def next(state: Option[Map[Int, Int]]): Option[Map[Int, Int]] =
      state.flatMap(nextState)

    def nextState(state: Map[Int, Int]): Option[Map[Int, Int]] = {
      val p = state.getOrElse(program.pointer, 0)
      program.instructions.lift(p)
        .map(Day16.applyInstruction(_, state))
        .map(s => s.updated(program.pointer, s.getOrElse(program.pointer, 0) + 1))
    }

    LazyList.iterate[Option[Map[Int, Int]]](Option(startingState))(next).takeWhile(_.isDefined).map(_.get)
  }

  override def part1(program: Program): Int =
    run(program, Map.empty).last.getOrElse(0, 0)

  // This part is infeasible to calculate with the approach used in part 1 - we have to look at the instructions manually and uncover the underlying problem
  // The instructions translate to a sum of divisors problem (after an initial set up phase)
  // The divisors are found in an increasing fashion and their sum is cumulatively collected in register 0
  override def part2(program: Program): Int = {
    // Changing register 0 from 0 to 1 changes the target (in my case from 955 to 10551355)
    // Instruction 33 sets up the target in the relevant register (in my case register 5)
    // So we can run the program until then and find our solution
    val instruction = 33
    val state = run(program, Map.empty.updated(0, 1)).dropWhile(_.getOrElse(2, 0) != instruction).drop(1).head
    val target = state.getOrElse(program.instructions(instruction).c, 0)
    Maths.divisors(target).sum
  }
}
