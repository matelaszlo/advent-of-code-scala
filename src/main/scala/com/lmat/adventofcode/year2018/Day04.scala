package com.lmat.adventofcode.year2018

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneOffset}

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2018.Day04Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.countElements

import scala.annotation.tailrec
import scala.util.Try

object Day04Definitions {
  case class Note(timestamp: LocalDateTime, note: String)
  case class SleepSchedule(date: LocalDate, minutesAsleep: Seq[Int])
}

object Day04 extends CommonPuzzle[Seq[Note], Map[Int, Seq[SleepSchedule]], Int, Int] {
  override def parse(resource: String): Seq[Note] = readResource(resource).flatMap(parseNote)

  def parseNote(row: String): Option[Note] = {
    val pattern = "\\[(.*)\\] (.*)".r
    row match {
      case pattern(timeS, note) => Try(LocalDateTime.parse(timeS, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))).toOption.map(Note(_, note))
      case _                    => None
    }
  }

  override def preProcess(notes: Seq[Note]): Map[Int, Seq[SleepSchedule]] =
    buildSleepScheduleMap(notes)

  /**
    * The majority of this puzzle is building up the sleep schedule per guard map
    * This is mad challenging as the rows rely on information from previous rows and also have to be pre-sorted
    */
  def buildSleepScheduleMap(notes: Seq[Note]): Map[Int, Seq[SleepSchedule]] = {
    val guardPattern = "Guard #(.*?) begins shift".r
    val sleepPattern = "falls asleep".r
    val wakePattern = "wakes up".r

    def addToMap(map: Map[Int, Seq[SleepSchedule]])(id: Int, date: LocalDate, start: Int, end: Int): Map[Int, Seq[SleepSchedule]] =
      map.updated(id, map.getOrElse(id, Seq()) :+ SleepSchedule(date, start until end))

    @tailrec
    def iterate(remaining: Seq[Note], tempId: Option[Int], tempStart: Option[Int], built: Map[Int, Seq[SleepSchedule]]): Map[Int, Seq[SleepSchedule]] = remaining match {
      case Note(time, note) +: rest =>
        note match {
          case guardPattern(idS) =>
            iterate(rest, Try(idS.toInt).toOption, None, built)
          case sleepPattern() =>
            iterate(rest, tempId, Some(time.getMinute), built)
          case wakePattern() =>
            iterate(rest, tempId, None, addToMap(built)(tempId.get, time.toLocalDate, tempStart.get, time.getMinute))
        }
      case _ => built
    }

    val sorted = notes.sortBy(_.timestamp.atOffset(ZoneOffset.UTC).toEpochSecond)
    iterate(sorted, None, None, Map())
  }

  override def part1(sleepScheduleMap: Map[Int, Seq[SleepSchedule]]): Int = {
    // Find the guard that has the most minutes asleep
    val (id, sleepSchedule) = sleepScheduleMap.maxBy{ case (_, v) => v.map(_.minutesAsleep.size).sum}
    //Find the minute that guard spends asleep the most
    val mostFrequentMinute  = countElements(sleepSchedule.flatMap(_.minutesAsleep)).maxBy(_._2)._1
    id * mostFrequentMinute
  }

  override def part2(sleepScheduleMap: Map[Int, Seq[SleepSchedule]]): Int = {
    // Find the most frequently asleep minute per guard
    val mostFrequentMinuteMap = sleepScheduleMap.mapValues(sleepSchedules => countElements(sleepSchedules.flatMap(_.minutesAsleep)).maxBy(_._2))
    // Find the most frequent minute and guard globally
    val (id, (minute, _)) = mostFrequentMinuteMap.maxBy(_._2._2)
    id * minute
  }
}
