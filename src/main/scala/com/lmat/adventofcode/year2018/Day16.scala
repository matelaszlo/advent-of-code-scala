package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day16Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day16Definitions {
  case class InstructionTemplate(opcode: Int, inputL: Int, inputR: Int, output: Int)
  case class CpuSample(before: Seq[Int], instruction: InstructionTemplate, after: Seq[Int])
  case class Input(cpuSamples: Seq[CpuSample], instructions: Seq[InstructionTemplate])

  case class Instruction(name: String, a: Int, b: Int, c: Int)
}

object Day16 extends SimpleCommonPuzzle[Input, Int, Int] {
  override def parse(resource: String): Input = parseInput(readResource(resource))

  def parseInput(rows: Seq[String]): Input = {
    val (cpuSamples, instructions) = rows.sliding(4, 4).span { case h +: _ => h.nonEmpty }
    Input(cpuSamples.flatMap(parseCpuSample).toSeq, instructions.flatten.flatMap(parseInstruction).toSeq)
  }

  def parseCpuSample(rows: Seq[String]): Option[CpuSample] = rows match {
    case beforeS +: instructionS +: afterS +: _ => for {
      before      <- parseState(beforeS, "Before")
      instruction <- parseInstruction(instructionS)
      after       <- parseState(afterS, "After")
    } yield CpuSample(before, instruction, after)
  }

  def parseState(row: String, time: String): Option[Seq[Int]] = {
    val pattern = s"$time:.*?\\[(.*?)\\]".r
    row match {
      case pattern(valuesS) => Some(valuesS.split(", ").flatMap(s => Try(s.toInt).toOption))
      case _ => None
    }
  }

  def parseInstruction(row: String): Option[InstructionTemplate] = row.split(" ").toSeq match {
    case oS +: aS +: bS +: cS +: Seq() => (for {
      o <- Try(oS.toInt)
      a <- Try(aS.toInt)
      b <- Try(bS.toInt)
      c <- Try(cS.toInt)
    } yield InstructionTemplate(o, a, b, c)).toOption
    case _ => None
  }

  lazy val instructionNames: Set[String] =
    Set(
      "addr", "addi",
      "mulr", "muli",
      "banr", "bani",
      "borr", "bori",
      "setr", "seti",
      "gtir", "gtri", "gtrr",
      "eqir", "eqri", "eqrr")

  override def part1(input: Input): Int =
    input.cpuSamples.count(matchingInstructions(instructionNames)(_).size >= 3)

  def matchingInstructions(instructionNames: Set[String])(cpuSample: CpuSample): Set[Instruction] = {
    val instructions = toInstructions(instructionNames)(cpuSample.instruction)
    instructions.filter(instruction => applyInstruction(instruction, cpuSample.before) == cpuSample.after)
  }

  def toInstructions(instructions: Set[String])(instructionTemplate: InstructionTemplate): Set[Instruction] = {
    val InstructionTemplate(_, inputL, inputR, output) = instructionTemplate
    instructions.map(name => Instruction(name, inputL, inputR, output))
  }

  def applyInstruction(instruction: Instruction, state: Seq[Int]): Seq[Int] = {
    val stateMap: Map[Int, Int] = state.zipWithIndex.map{ case (v, i) => (i, v)}.toMap
    applyInstruction(instruction, stateMap).toSeq.sortBy(_._1).map(_._2)
  }

  def applyInstruction(instruction: Instruction, state: Map[Int, Int]): Map[Int, Int] = {
    def register(name: Int): Int = state.getOrElse(name, 0)

    instruction match {
      case Instruction("addr", aR, bR, cR) => state.updated(cR, register(aR) + register(bR))
      case Instruction("addi", aR, b, cR)  => state.updated(cR, register(aR) + b)
      case Instruction("mulr", aR, bR, cR) => state.updated(cR, register(aR) * register(bR))
      case Instruction("muli", aR, b, cR)  => state.updated(cR, register(aR) * b)

      case Instruction("banr", aR, bR, cR) => state.updated(cR, register(aR) & register(bR))
      case Instruction("bani", aR, b, cR)  => state.updated(cR, register(aR) & b)
      case Instruction("borr", aR, bR, cR) => state.updated(cR, register(aR) | register(bR))
      case Instruction("bori", aR, b, cR)  => state.updated(cR, register(aR) | b)

      case Instruction("setr", aR, _, cR)  => state.updated(cR, register(aR))
      case Instruction("seti", a, _, cR)   => state.updated(cR, a)

      case Instruction("gtir", a, bR, cR)  => state.updated(cR, if(a > register(bR)) 1 else 0)
      case Instruction("gtri", aR, b, cR)  => state.updated(cR, if(register(aR) > b) 1 else 0)
      case Instruction("gtrr", aR, bR, cR) => state.updated(cR, if(register(aR) > register(bR)) 1 else 0)

      case Instruction("eqir", a, bR, cR)  => state.updated(cR, if(a == register(bR)) 1 else 0)
      case Instruction("eqri", aR, b, cR)  => state.updated(cR, if(register(aR) == b) 1 else 0)
      case Instruction("eqrr", aR, bR, cR) => state.updated(cR, if(register(aR) == register(bR)) 1 else 0)

      case _                               => throw new IllegalArgumentException(s"Illegal instruction: $instruction")
    }
  }

  override def part2(input: Input): Int = {
    val opCodeMap = buildOpCodeMap(input.cpuSamples)
    val instructions = input.instructions.map(ins => Instruction(opCodeMap(ins.opcode), ins.inputL, ins.inputR, ins.output))

    val result = instructions.foldLeft(Map[Int, Int]())((state, ins) => applyInstruction(ins, state))
    result.getOrElse(0, 0)
  }

  /**
    * Luckily given the input we can build the opcode to instruction name map recursively
    * By finding a Cpu sample that can only be satisfied by one instruction
    * And then removing that instruction and all relevant samples from the pool
    */
  def buildOpCodeMap(cpuSamples: Seq[CpuSample]): Map[Int, String] = {

    @tailrec
    def iterate(remainingSamples: Set[CpuSample], remainingInstructions: Set[String], builtMap: Map[Int, String]): Map[Int, String] =
      if(remainingInstructions.isEmpty) builtMap
      else {
        remainingSamples.map(sample => (sample, matchingInstructions(remainingInstructions)(sample).map(_.name))).find(_._2.size == 1) match {
          case Some((CpuSample(_, InstructionTemplate(opcode, _, _, _), _), instructions)) =>
            iterate(remainingSamples.filterNot(_.instruction.opcode == opcode), remainingInstructions -- instructions, builtMap.updated(opcode, instructions.head))
          case None => builtMap
        }
      }

    iterate(cpuSamples.toSet, instructionNames, Map())
  }
}
