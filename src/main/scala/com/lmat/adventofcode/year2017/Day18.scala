package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.adventofcode.year2017.Day18Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try

object Day18Definitions {
  sealed trait Instruction
  case class Sound    (base: String)                extends Instruction
  case class Set      (base: String, value: String) extends Instruction
  case class Add      (base: String, value: String) extends Instruction
  case class Multiply (base: String, value: String) extends Instruction
  case class Modulo   (base: String, value: String) extends Instruction
  case class Recover  (base: String)                extends Instruction
  case class Jump     (base: String, value: String) extends Instruction
  case class Send     (base: String)                extends Instruction
  case class Receive  (base: String)                extends Instruction
}

object Day18 extends SimpleMultiPuzzle[Seq[Instruction], Long, Seq[Instruction], Int] {
  override def parse1(resource: String): Seq[Instruction] = readResource(resource).flatMap(row => parseInstruction(row))

  override def parse2(resource: String): Seq[Instruction] = readResource(resource).flatMap(row => parseInstruction(row, true))

  def parseInstruction(line: String, isSending: Boolean = false): Option[Instruction] = {
    val soundTest     = """snd\s+(.+)""".r
    val setTest       = """set\s+(.+)\s+(.+)""".r
    val addTest       = """add\s+(.+)\s+(.+)""".r
    val multiplyTest  = """mul\s+(.+)\s+(.+)""".r
    val moduloTest    = """mod\s+(.+)\s+(.+)""".r
    val recoverTest   = """rcv\s+(.+)""".r
    val jumpTest      = """jgz\s+(.+)\s+(.+)""".r

    line match {
      case soundTest(base)            => if (isSending) Some(Send(base)) else Some(Sound(base))
      case setTest(base, value)       => Some(Set(base, value))
      case addTest(base, value)       => Some(Add(base, value))
      case multiplyTest(base, value)  => Some(Multiply(base, value))
      case moduloTest(base, value)    => Some(Modulo(base, value))
      case recoverTest(base)          => if (isSending) Some(Receive(base)) else Some(Recover(base))
      case jumpTest(base, value)      => Some(Jump(base, value))
      case _                          => None
    }
  }

  case class State(registers: Map[String, Long], current: Int, played: Seq[Long], recovered: Seq[Long], send: Queue[Long], sent: Int, received: Queue[Long], waiting: Boolean) {
    def register(name: String): Long = Try(name.toLong).getOrElse(registers.getOrElse(name, 0))

    def update(name: String, value: String): State = copy(registers = registers.updated(name, register(value)))
    def update(name: String, value: String, function: (Long, Long) => Long): State = copy(registers = registers.updated(name, function(register(name), register(value))))

    def next: State = copy(current = current + 1)
    def jump(value: String): State = copy(current = current + register(value).toInt)

    def play(value: String): State = copy(played = played :+ register(value))
    def recover: State = copy(recovered =  played.last +: recovered)

    def send(value: String): State = copy(send = send.enqueue(register(value)), sent = sent + 1)
    def receive(name: String): State = {
      val (i, q) = received.dequeue
      copy(registers = registers.updated(name, i), received = q)
    }
  }

  val empty: State = State(Map(), 0, Seq(), Seq(), Queue.empty, 0, Queue.empty, false)

  override def part1(instructions: Seq[Instruction]): Long =
    firstRecoveredValue(empty, instructions)

  @tailrec
  def firstRecoveredValue(state: State, instructions: Seq[Instruction]): Long =
    if (isFinished(state, instructions)) state.recovered.head
    else if(state.recovered.nonEmpty) state.recovered.head
    else firstRecoveredValue(applyInstruction(state, instructions(state.current)), instructions)

  def applyInstruction(state: State, instruction: Instruction): State = instruction match {
    case Sound   (base)         => state.play(base).next
    case Set     (base, value)  => state.update(base, value).next
    case Add     (base, value)  => state.update(base, value, _ + _).next
    case Multiply(base, value)  => state.update(base, value, _ * _).next
    case Modulo  (base, value)  => state.update(base, value, _ % _).next
    case Recover (base)         => if(state.register(base) != 0) state.recover.next else state.next
    case Jump    (base, value)  => if(state.register(base) > 0)  state.jump(value)  else state.next
    case Send    (base)         => state.send(base).next
    case Receive (base)         => if(state.received.isEmpty) state.copy(waiting = true) else state.receive(base).next.copy(waiting = false)
  }

  def isFinished(state: State, instructions: Seq[Instruction]): Boolean = state.current < 0 || state.current >= instructions.size

  /**
    * Note that Instructions are modelled in a way that we can reuse the ADT, parsing logic and apply between the two parts
    */
  override def part2(instructions: Seq[Instruction]): Int =
    program1SendCount(empty.update("p", "0"), empty.update("p", "1"), instructions)

  @tailrec
  def program1SendCount(p0: State, p1:State, instructions: Seq[Instruction]): Int =
    if(isFinished(p0, instructions) && isFinished(p1, instructions)) p1.sent
    else if(p0.waiting && p1.waiting) p1.sent
    else {
      val (p0new, p1new) = reconcile(applyInstruction(p0, instructions(p0.current)), applyInstruction(p1, instructions(p1.current)))
      program1SendCount(p0new, p1new, instructions)
    }

  def reconcile(p0: State, p1:State): (State, State) ={
    val (p1new, p0new) = move(p1, p0)
    move(p0new, p1new)
  }

  def move(from: State, to: State): (State, State) =
    if (from.send.nonEmpty) {
      val (i, q) = from.send.dequeue
      (from.copy(send = q), to.copy(received = to.received.enqueue(i)))
    } else (from, to)
}
