package com.lmat.adventofcode.year2018

import java.time.{LocalDate, LocalDateTime, Month}

import com.lmat.adventofcode.year2018.Day04._
import com.lmat.adventofcode.year2018.Day04Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day04Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawNotes =
    s"""[1518-11-01 00:00] Guard #10 begins shift
       |[1518-11-01 00:05] falls asleep
       |[1518-11-01 00:25] wakes up
       |[1518-11-01 00:30] falls asleep
       |[1518-11-01 00:55] wakes up
       |[1518-11-01 23:58] Guard #99 begins shift
       |[1518-11-02 00:40] falls asleep
       |[1518-11-02 00:50] wakes up
       |[1518-11-03 00:05] Guard #10 begins shift
       |[1518-11-03 00:24] falls asleep
       |[1518-11-03 00:29] wakes up
       |[1518-11-04 00:02] Guard #99 begins shift
       |[1518-11-04 00:36] falls asleep
       |[1518-11-04 00:46] wakes up
       |[1518-11-05 00:03] Guard #99 begins shift
       |[1518-11-05 00:45] falls asleep
       |[1518-11-05 00:55] wakes up""".stripMargin

  val notes = Seq(
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 1, 0, 0),"Guard #10 begins shift"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 1, 0, 5),"falls asleep"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 1, 0, 25),"wakes up"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 1, 0, 30),"falls asleep"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 1, 0, 55),"wakes up"),

    Note(LocalDateTime.of(1518, Month.NOVEMBER, 1, 23, 58),"Guard #99 begins shift"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 2, 0, 40),"falls asleep"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 2, 0, 50),"wakes up"),

    Note(LocalDateTime.of(1518, Month.NOVEMBER, 3, 0, 5),"Guard #10 begins shift"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 3, 0, 24),"falls asleep"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 3, 0, 29),"wakes up"),

    Note(LocalDateTime.of(1518, Month.NOVEMBER, 4, 0, 2),"Guard #99 begins shift"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 4, 0, 36),"falls asleep"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 4, 0, 46),"wakes up"),

    Note(LocalDateTime.of(1518, Month.NOVEMBER, 5, 0, 3),"Guard #99 begins shift"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 5, 0, 45),"falls asleep"),
    Note(LocalDateTime.of(1518, Month.NOVEMBER, 5, 0, 55),"wakes up"))

  test("Day04 - Parse") {
    assert(rawNotes.split("\n").toSeq.flatMap(parseNote) == notes)
  }

  val sleepScheduleMap = Map(
    10 -> List(
      SleepSchedule(LocalDate.of(1518, Month.NOVEMBER, 1), 5 until 25),
      SleepSchedule(LocalDate.of(1518, Month.NOVEMBER, 1), 30 until 55),
      SleepSchedule(LocalDate.of(1518, Month.NOVEMBER, 3), 24 until 29)),
    99 -> List(
      SleepSchedule(LocalDate.of(1518, Month.NOVEMBER, 2), 40 until 50),
      SleepSchedule(LocalDate.of(1518, Month.NOVEMBER, 4), 36 until 46),
      SleepSchedule(LocalDate.of(1518, Month.NOVEMBER, 5), 45 until 55)))

  test("Day04 - Build Sleep Schedule Map") {
    assert(buildSleepScheduleMap(notes) == sleepScheduleMap)
  }

  test("Day04 - Part 1") {
    assert(part1(sleepScheduleMap) == 240)
  }

  test("Day04 - Part 2") {
    assert(part2(sleepScheduleMap) == 4455)
  }
}
