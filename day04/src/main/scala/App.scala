import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class SleepRecord(day: Int, month: Int, guard: Int) {
  val minutes = new Array[Int](60)
  override def toString: String = f"$month%02d-$day%02d $guard, total sleep ${minutes.sum}"
}

object App {

  def main(args: Array[String]): Unit = {
    val sleepRecords = parse(Source.fromResource("input.txt").getLines().toList)
    println(part1(sleepRecords))
    println(part2(sleepRecords))
  }

  def parse(lines: List[String]): List[SleepRecord] = {
    var list = new ListBuffer[SleepRecord]()
    var currentSleepTime = 0
    var currentSleepRecord = None: Option[SleepRecord]
    //can be sorted lexographically
    for (line <- lines.sortBy(_.split(']').head)) {
      var dateTime = parseDateTime(line)
      if (dateTime.getHour != 0) { //is it before midnight?
        dateTime = dateTime.plusDays(1) //then the shift starts the next day
      }
      line match {
        case s if s.contains("Guard") =>
          currentSleepRecord = Some(SleepRecord(dateTime.getDayOfMonth, dateTime.getMonthValue, parseGuard(s)))
        case s if s.contains("falls asleep") =>
          currentSleepTime = dateTime.getMinute
        case s if s.contains("wakes up") =>
          for (i <- currentSleepTime until dateTime.getMinute if currentSleepRecord.isDefined) {
            currentSleepRecord.get.minutes(i) = 1
          }
      }
      if (currentSleepRecord.isDefined)
        list += currentSleepRecord.get
    }
    list.toList
  }

  def parseGuard(line: String): Int = {
    val pattern = ".*Guard #(\\d+) begins shift$".r
    val pattern(n) = line
    n.toInt
  }

  def parseDateTime(line: String): LocalDateTime = {
    LocalDateTime.parse(line.substring(1, line.indexOf(']')), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
  }

  def part1(list: List[SleepRecord]): Int = {
    //the guard that sleeps the most
    val guard = list.groupBy(_.guard).map { case (k, v) => (k, v.foldLeft(0)(_ + _.minutes.sum)) }.maxBy(_._2)._1
    //the minute that this guard sleeps on the most
    val minutes = list.filter(_.guard == guard).map(_.minutes).foldLeft(new Array[Int](60))({
      _.zipAll(_, 0, 0) map { case (a, b) => a + b }
    })
    minutes.indexOf(minutes.max) * guard
  }

  def part2(list: List[SleepRecord]): Int = {
    val minutes = list.map(_.minutes).foldLeft(new Array[Int](60))({
      _.zipAll(_, 0, 0) map { case (a, b) => a + b }
    })
    //the minute most slept on
    val minute = minutes.indexOf(minutes.max)
    //the guard which sleeps that most on that minute
    val guard = list.filter(_.minutes(minute) != 0).groupBy(_.guard).maxBy(_._2.size)._1
    minute * guard
  }

}
